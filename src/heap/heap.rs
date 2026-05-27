use std::ops::{Deref, DerefMut};

use silver_oxide::{parse::BinOp, HashMap};

use crate::{error::ExpressionError, pure::{EGraph, EggExpKind, PathCondition}, translator::VerificationState};

pub type Mutation = u32;
pub type TemporaryInner = u32;
pub type Temporary = (bool, TemporaryInner);

/// A set of chunks for multiple resources.
#[derive(Debug, Default, Clone)]
pub struct Heap(pub(super) HashMap<egg::Id, CTreeChunk>);

impl Deref for Heap {
    type Target = HashMap<egg::Id, CTreeChunk>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Heap {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// An heap chunk under the current path pc and a tree of chunks
/// under extended path conditions.
#[derive(Debug, Clone)]
pub struct CTreeChunk {
    pub chunk: HeapChunk,
    pub updates: Vec<CTreeChunkUpdate>,
}

pub struct CTreeChunkCleanup(Vec<usize>);

impl CTreeChunk {
    fn new(egraph: &mut EGraph, value: Option<egg::Id>, resource: egg::Id) -> Self {
        let symbolic_value = value.unwrap_or_else(|| egraph.next_heap_value(resource));
        let permission = egraph.none();
        let chunk = HeapChunk { permission, symbolic_value };
        Self { chunk, updates: Vec::new() }
    }

    pub fn get_chunk_cleanup(&mut self, egraph: &mut EGraph, pc: &PathCondition, skip: Option<Mutation>, overwrites: bool) -> HeapChunk {
        let (chunk, cleanup) = self.get_chunk(egraph, pc, skip, overwrites);
        self.cleanup(cleanup);
        chunk
    }

    pub fn get_chunk(&self, egraph: &mut EGraph, pc: &PathCondition, skip: Option<Mutation>, overwrites: bool) -> (HeapChunk, CTreeChunkCleanup) {
        let mut curr_condition = pc.clone();
        let mut chunks: Vec<(egg::Id, HeapChunk)> = Vec::new();
        let mut to_remove = CTreeChunkCleanup(Vec::new());
        // println!("ALL {:?} // {:?}", self.chunk, self.updates);
        for (i, ct_chunk) in self.updates.iter().enumerate().rev() {
            // println!("Checking {ct_chunk:?} under {curr_condition:?}");
            if skip.zip(ct_chunk.mutation).is_some_and(|(a, b)| a == b) {
                continue;
            }
            // NOTE: If we're a temporary permission then we must have been
            // updated within the same expression (and the PC can only get stronger).
            let merge = if ct_chunk.temporary.is_some() {
                Ok(true)
            } else {
                curr_condition.should_merge(egraph, &ct_chunk.pc)
            };
            // println!("Got {merge:?}");
            match merge {
                Ok(true) => return (self.return_chunk(egraph, chunks, ct_chunk.chunk, pc.clone()), to_remove),
                Ok(false) => {
                    to_remove.0.push(i);
                    continue
                }
                Err(cond) => {
                    if overwrites {
                        to_remove.0.push(i);
                    }
                    curr_condition.merge(egraph, &ct_chunk.pc);
                    chunks.push((cond, ct_chunk.chunk));
                }
            }
        }
        (self.return_chunk(egraph, chunks, self.chunk, pc.clone()), to_remove)
    }

    fn return_chunk(&self, egraph: &mut EGraph, chunks: Vec<(egg::Id, HeapChunk)>, chunk: HeapChunk, pc: PathCondition) -> HeapChunk {
        chunks.into_iter().rev().fold(chunk, |acc, (pc, chunk)| {
            let permission = egraph.add_real(EggExpKind::Ternary([pc, chunk.permission, acc.permission]));
            let symbolic_value = egraph.add_real(EggExpKind::Ternary([pc, chunk.symbolic_value, acc.symbolic_value]));
            HeapChunk { permission, symbolic_value }
        })
    }

    fn cleanup(&mut self, cleanup: CTreeChunkCleanup) {
        if !cleanup.0.is_empty() {
            let mut to_remove = cleanup.0.into_iter().rev();
            let mut i = 0;
            let mut j = to_remove.next();
            self.updates.retain(|_| {
                let curr = i;
                i = i + 1;
                if j.is_some_and(|j| curr == j) {
                    j = to_remove.next();
                    false
                } else {
                    true
                }
            })
        }
    }

    pub fn add_update(&mut self, egraph: &mut EGraph, pc: &PathCondition, skip: Option<Mutation>) -> &mut HeapChunk {
        // TODO: I changed this fn and didn't think about if it's correct when
        // skip is Some.
        assert!(skip.is_none());
        let chunk = self.get_chunk_cleanup(egraph, pc, skip, true);
        if pc.is_true(egraph) {
            assert_eq!(self.updates.len(), 0);
            self.chunk = chunk;
            &mut self.chunk
        } else {
            let update = CTreeChunkUpdate { pc: pc.clone(), chunk, mutation: None, temporary: None };
            self.updates.push(update);
            &mut self.updates.last_mut().unwrap().chunk
        }
    }
}

#[derive(Debug, Clone)]
pub struct CTreeChunkUpdate {
    pc: PathCondition,
    mutation: Option<Mutation>,
    temporary: Option<TemporaryInner>,
    chunk: HeapChunk,
}

// impl CTreeChunkUpdate {
//     fn assert_not_negative_permission(&self, egraph: &mut EGraph, amount: egg::Id, decls: &VerificationState<'_>) -> Result<(), ExpressionError> {
//         self.chunk.assert_not_negative_permission(&self.pc, egraph, amount, decls)
//     }

//     pub fn get_chunk_unsafe(&self) -> HeapChunk {
//         self.chunk
//     }
// }

impl HeapChunk {
    fn assert_not_negative_permission(&self, pc: &PathCondition, egraph: &mut EGraph, amount: egg::Id, decls: &VerificationState<'_>) -> Result<(), ExpressionError> {
        let perm_not_negative = egraph.add_bool(EggExpKind::BinOp(BinOp::Le, [egraph.none(), amount]));
        let tmp = pc.assert(egraph, perm_not_negative, "perm_not_negative", decls).map_err(ExpressionError::negative_permission);
        // println!("assert_not_negative_permission: {amount:?} -> {perm_not_negative:?} = {:?}", tmp);
        tmp
    }

    pub fn get_symbolic_value(&self, pc: &PathCondition, egraph: &mut EGraph, decls: &VerificationState<'_>) -> Result<egg::Id, ExpressionError> {
        let perm_positive = egraph.add_bool(EggExpKind::BinOp(BinOp::Lt, [egraph.none(), self.permission]));
        pc.assert(egraph, perm_positive, "perm_positive", decls).map_err(|err| {
            println!("{self:?} / {err:?}");
            panic!();
            ExpressionError::read_missing(err)
        })?;
        Ok(self.symbolic_value)
    }

    pub fn get_permission(&self) -> egg::Id {
        self.permission
    }

    pub fn add_permission(&mut self, pc: &PathCondition, egraph: &mut EGraph, amount: egg::Id, value: Option<egg::Id>, bound: Option<egg::Id>, temporary: Option<Temporary>, decls: &VerificationState<'_>) -> Result<egg::Id, ExpressionError> {
        self.assert_not_negative_permission(pc, egraph, amount, decls)?;
        // TODO:
        assert!(temporary.is_none());
        // self.temporary = temporary.map(|(_, temporary)| temporary);
        self.permission = egraph.add_real(EggExpKind::BinOp(BinOp::Plus, [self.permission, amount]));

        if let Some(value) = value {
            egraph.equate(self.symbolic_value, value, "add permission value");
        }

        // println!("add_permission: {:?} -> {bound:?}", self.chunk.permission);
        if let Some(bound) = bound {
            let bound = egraph.add_bool(EggExpKind::BinOp(BinOp::Le, [self.permission, bound]));
            let bound = pc.condition(egraph, bound);
            // egraph.saturate();
            // egraph.egraph.dot().with_config_line("ranksep=5.5").to_pdf("pre_bound.pdf").unwrap();
            egraph.assume(bound, "resource bound");
            // egraph.egraph.dot().with_config_line("ranksep=5.5").to_pdf("post_bound.pdf").unwrap();
        }
        Ok(self.symbolic_value)
    }

    pub fn remove_permission(&mut self, pc: &PathCondition, egraph: &mut EGraph, amount: egg::Id, mutation: Option<Mutation>, temporary: Option<Temporary>, decls: &VerificationState<'_>) -> Result<egg::Id, ExpressionError> {
        self.assert_not_negative_permission(pc, egraph, amount, decls)?;
        // TODO:
        assert!(temporary.is_none() && mutation.is_none());
        // self.temporary = temporary.map(|(_, temporary)| temporary);
        // self.mutation = mutation;
        self.permission = egraph.add_real(EggExpKind::BinOp(BinOp::Minus, [self.permission, amount]));
        self.assert_not_negative_permission(pc, egraph, self.permission, decls)?;
        let old_value = self.symbolic_value;

        let non_zero = egraph.add_bool(EggExpKind::BinOp(BinOp::Lt, [egraph.none(), self.permission]));
        // TODO: use the old or the new pc here?
        if let Err(_) = pc.assert_lite(egraph, non_zero) {
            let old_value_ty = egraph.ty(old_value);
            let new_val = egraph.next_heap_value_like(old_value_ty);
            self.symbolic_value = egraph.add(old_value_ty, EggExpKind::Ternary([non_zero, old_value, new_val]));
        }
        Ok(old_value)
    }

    /// Try to update the symbolic value of the chunk. Returns `Ok(())` if
    /// permission is equal to `write` and the symbolic value was updated, else
    /// returns `Err(permission_check_id)`.
    pub fn update_value(&mut self, pc: &PathCondition, egraph: &mut EGraph, symbolic_value: egg::Id, decls: &VerificationState<'_>) -> Result<(), ExpressionError> {
        let perm_write = egraph.add_bool(EggExpKind::BinOp(BinOp::Eq, [egraph.write(), self.permission]));
        pc.assert(egraph, perm_write, "write_perm", decls).map_err(ExpressionError::write_missing)?;
        self.symbolic_value = symbolic_value;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct HeapChunk {
    pub permission: egg::Id,
    pub symbolic_value: egg::Id,
}

impl Heap {
    fn get_resource(&mut self, egraph: &mut EGraph, resource: egg::Id, pc: &PathCondition) -> Result<&mut CTreeChunk, ExpressionError> {
        let resource = egraph.normalise(resource);
        // self.normalise(egraph);
        self.get_mut(&resource).ok_or(ExpressionError::resource_not_found(resource))
    }

    fn get_chunk(&mut self, egraph: &mut EGraph, resource: egg::Id, pc: &PathCondition, skip: Option<Mutation>) -> Result<HeapChunk, ExpressionError> {
        let ct_chunk = self.get_resource(egraph, resource, pc)?;
        Ok(ct_chunk.get_chunk_cleanup(egraph, pc, skip, false))
    }

    pub fn get_symbolic_value(&mut self, egraph: &mut EGraph, resource: egg::Id, pc: &PathCondition, skip: Option<Mutation>, decls: &VerificationState<'_>) -> Result<egg::Id, ExpressionError> {
        let chunk = self.get_chunk(egraph, resource, pc, skip)?;
        // println!("get_symbolic_value: {:#?}", self.0.get(&resource).unwrap());
        chunk.get_symbolic_value(pc, egraph, decls)
    }

    /// Returns `(value, permission)` but the value is only guaranteed to be
    /// valid if the permission is positive: it is up to the caller to check
    /// this. The `value` should only be used for snapshot purposes.
    pub fn get_permission(&mut self, egraph: &mut EGraph, resource: egg::Id, pc: &PathCondition, skip: Option<Mutation>) -> Result<(egg::Id, egg::Id), ExpressionError> {
        let chunk = self.get_chunk(egraph, resource, pc, skip)?;
        // println!("get_permission {resource:?}/{pc:?}: {:#?}", chunk);
        Ok((chunk.symbolic_value, chunk.get_permission()))
    }

    pub fn update_symbolic_value(&mut self, egraph: &mut EGraph, resource: egg::Id, symbolic_value: egg::Id, pc: &PathCondition, decls: &VerificationState<'_>) -> Result<(), ExpressionError> {
        let resource = self.get_resource(egraph, resource, pc)?;
        let update = resource.add_update(egraph, pc, None);
        update.update_value(pc, egraph, symbolic_value, decls)
    }

    /// Add `permission` amount to a heap chunk (identified by `resource`) under
    /// the given `pc`. If `bound` is `Some`, the permission will be
    /// assumed to be less than or equal to `bound`. A new symbolic value is
    /// created if we had no permission before. Returns the symbolic value of
    /// the chunk.
    pub fn add_permission(&mut self, egraph: &mut EGraph, resource: egg::Id, permission: egg::Id, pc: &PathCondition, value: Option<egg::Id>, bound: Option<egg::Id>, config: PermissionConfig<()>, decls: &VerificationState<'_>) -> Result<egg::Id, ExpressionError> {
        // println!("add_permission: {resource:?} -> {permission:?} = {value:?} pc: {pc:?} ({})", pc.is_true(egraph));
        let resource = egraph.normalise(resource);
        let resource = self.entry(resource).or_insert_with(|| CTreeChunk::new(egraph, value, resource));
        if let PermissionConfig::Temporary { temporary: (false, temporary), .. } = config {
            let last = resource.updates.pop().unwrap();
            assert_eq!(last.temporary, Some(temporary));
            return Ok(last.chunk.symbolic_value)
        }
        let update = resource.add_update(egraph, pc, config.skip());
        update.add_permission(pc, egraph, permission, value, bound, config.temporary(), decls)
    }

    /// Remove `permission` amount from a heap chunk (identified by `resource`)
    /// under the given `pc`. The symbolic value of the chunk is havoc'd
    /// if the permission becomes zero. Returns the old symbolic value before
    /// the update.
    pub fn remove_permission(&mut self, egraph: &mut EGraph, resource: egg::Id, permission: egg::Id, pc: &PathCondition, config: PermissionConfig<Mutation>, decls: &VerificationState<'_>) -> Result<egg::Id, ExpressionError> {
        let resource = self.get_resource(egraph, resource, pc)?;
        if let PermissionConfig::Temporary { temporary: (false, temporary), .. } = config {
            let last = resource.updates.pop().unwrap();
            assert_eq!(last.temporary, Some(temporary));
            return Ok(last.chunk.symbolic_value)
        }
        let update = resource.add_update(egraph, pc, config.skip());
        update.remove_permission(pc, egraph, permission, config.mutation(), config.temporary(), decls)
    }

    pub fn chunks(&self) -> impl Iterator<Item = (egg::Id, &CTreeChunk)> {
        self.iter().map(|(resource, chunk)| (*resource, chunk))
    }

    pub fn kill_temporary(&mut self, temporary: TemporaryInner) {
        for chunk in self.values_mut() {
            if chunk.updates.last().is_some_and(|last| last.temporary == Some(temporary)) {
                chunk.updates.pop();
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum AddPermissionConfig {
    Temporary {
        temporary: Temporary,
        mutation: Option<Mutation>,
    },
    Mutation,
}

#[derive(Debug, Clone, Copy)]
pub enum PermissionConfig<T: Copy> {
    Temporary {
        temporary: Temporary,
        mutation: Option<Mutation>,
    },
    Mutation(T),
}

impl<T: Copy> PermissionConfig<T> {
    pub fn skip(&self) -> Option<Mutation> {
        match self {
            PermissionConfig::Temporary { mutation, .. } => *mutation,
            PermissionConfig::Mutation { .. } => None,
        }
    }
    pub fn mutation(&self) -> Option<T> {
        match self {
            PermissionConfig::Temporary { .. } => None,
            PermissionConfig::Mutation(mutation) => Some(*mutation),
        }
    }
    pub fn temporary(&self) -> Option<Temporary> {
        match self {
            PermissionConfig::Temporary { temporary, .. } => Some(*temporary),
            PermissionConfig::Mutation { .. } => None,
        }
    }
}
