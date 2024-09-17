use std::ops::{Deref, DerefMut};

use fxhash::FxHashMap;

use crate::{error::ExpressionError, exp::{BinOp, Exp, PathCondition}, pure::EGraph};

pub type Mutation = u32;
pub type TemporaryInner = u32;
pub type Temporary = (bool, TemporaryInner);

/// A set of chunks for multiple resources.
#[derive(Debug, Default, Clone)]
pub struct Heap(pub(super) FxHashMap<egg::Id, CTreeChunk>);

impl Deref for Heap {
    type Target = FxHashMap<egg::Id, CTreeChunk>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Heap {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// An heap chunk under the current path condition and a tree of chunks
/// under extended path conditions.
#[derive(Debug, Clone)]
pub struct CTreeChunk {
    pub chunk: HeapChunk,
    pub updates: Vec<CTreeChunkUpdate>,
}

impl CTreeChunk {
    pub fn get_chunk(&self, egraph: &mut EGraph, condition: PathCondition, skip: Option<Mutation>) -> CTreeChunkUpdate {
        let return_ = |egraph: &mut EGraph, chunks: Vec<(egg::Id, HeapChunk)>, chunk: HeapChunk| -> CTreeChunkUpdate {
            let chunk = chunks.into_iter().rev().fold(chunk, |acc, (condition, chunk)| {
                let permission = egraph.add(Exp::Ternary([condition, chunk.permission, acc.permission]));
                let symbolic_value = egraph.add(Exp::Ternary([condition, chunk.symbolic_value, acc.symbolic_value]));
                HeapChunk { permission, symbolic_value }
            });
            CTreeChunkUpdate { condition, mutation: None, temporary: None, chunk }
        };

        let mut curr_condition = condition;
        let mut chunks: Vec<(egg::Id, HeapChunk)> = Vec::new();
        for ct_chunk in self.updates.iter().rev() {
            if skip.zip(ct_chunk.mutation).is_some_and(|(a, b)| a == b) {
                continue;
            }
            match curr_condition.merge(egraph, ct_chunk.condition) {
                Ok(true) => return return_(egraph, chunks, ct_chunk.chunk),
                Ok(false) => continue,
                Err((new_pc, cond)) => {
                    chunks.push((cond, ct_chunk.chunk));
                    curr_condition = new_pc;
                }
            }
        }
        return_(egraph, chunks, self.chunk)
    }

    pub fn add_update(&mut self, egraph: &mut EGraph, condition: PathCondition, skip: Option<Mutation>) -> &mut CTreeChunkUpdate {
        let chunk = self.get_chunk(egraph, condition, skip);
        self.updates.push(chunk);
        self.updates.last_mut().unwrap()
    }
}

#[derive(Debug, Clone)]
pub struct CTreeChunkUpdate {
    condition: PathCondition,
    mutation: Option<Mutation>,
    temporary: Option<TemporaryInner>,
    chunk: HeapChunk,
}

impl CTreeChunkUpdate {
    fn assert_not_negative_permission(&self, egraph: &mut EGraph, amount: egg::Id) -> Result<(), ExpressionError> {
        let perm_not_negative = egraph.add_binop(silver_oxide::ast::BinOp::Le, egraph.none(), amount);
        let tmp = self.condition.assert(egraph, perm_not_negative).map_err(ExpressionError::negative_permission);
        println!("assert_not_negative_permission: {amount:?} -> {perm_not_negative:?} = {:?}", tmp);
        tmp
    }

    pub fn get_chunk_unsafe(&self) -> HeapChunk {
        self.chunk
    }

    pub fn get_symbolic_value(&self, egraph: &mut EGraph) -> Result<egg::Id, ExpressionError> {
        let perm_positive = egraph.add(Exp::BinOp(BinOp::Lt, [egraph.none(), self.chunk.permission]));
        self.condition.assert(egraph, perm_positive).map_err(|err| {
            println!("{self:?} / {err:?}");
            panic!();
            ExpressionError::read_missing(err)
        })?;
        Ok(self.chunk.symbolic_value)
    }

    pub fn get_permission(&self) -> egg::Id {
        self.chunk.permission
    }

    pub fn add_permission(&mut self, egraph: &mut EGraph, amount: egg::Id, value: Option<egg::Id>, bound: Option<egg::Id>, temporary: Option<Temporary>) -> Result<egg::Id, ExpressionError> {
        self.assert_not_negative_permission(egraph, amount)?;
        self.temporary = temporary.map(|(_, temporary)| temporary);
        self.chunk.permission = egraph.add(Exp::BinOp(BinOp::Plus, [self.chunk.permission, amount]));

        if let Some(value) = value {
            egraph.equate(self.chunk.symbolic_value, value, "add permission value");
        }

        // println!("add_permission: {:?} -> {bound:?}", self.chunk.permission);
        if let Some(bound) = bound {
            let bound = egraph.add_binop(silver_oxide::ast::BinOp::Le, self.chunk.permission, bound);
            let bound = self.condition.condition(egraph, bound);
            // egraph.saturate();
            // egraph.egraph.dot().with_config_line("ranksep=5.5").to_pdf("pre_bound.pdf").unwrap();
            egraph.assume(bound, "resource bound");
            // egraph.egraph.dot().with_config_line("ranksep=5.5").to_pdf("post_bound.pdf").unwrap();
        }
        Ok(self.chunk.symbolic_value)
    }

    pub fn remove_permission(&mut self, egraph: &mut EGraph, amount: egg::Id, mutation: Option<Mutation>, temporary: Option<Temporary>) -> Result<egg::Id, ExpressionError> {
        self.assert_not_negative_permission(egraph, amount)?;
        self.temporary = temporary.map(|(_, temporary)| temporary);
        self.mutation = mutation;
        self.chunk.permission = egraph.add_binop(silver_oxide::ast::BinOp::Minus, self.chunk.permission, amount);
        self.assert_not_negative_permission(egraph, self.chunk.permission)?;
        let old_value = self.chunk.symbolic_value;

        let non_zero = egraph.add(Exp::BinOp(BinOp::Lt, [egraph.none(), self.chunk.permission]));
        // TODO: use the old or the new condition here?
        if let Err(_) = self.condition.assert_lite(egraph, non_zero) {
            let new_val = egraph.next_symbolic_value(None);
            self.chunk.symbolic_value = egraph.add(Exp::Ternary([non_zero, self.chunk.symbolic_value, new_val]));
        }
        Ok(old_value)
    }

    /// Try to update the symbolic value of the chunk. Returns `Ok(())` if
    /// permission is equal to `write` and the symbolic value was updated, else
    /// returns `Err(permission_check_id)`.
    pub fn update_value(&mut self, egraph: &mut EGraph, symbolic_value: egg::Id) -> Result<(), ExpressionError> {
        let perm_write = egraph.add(Exp::BinOp(BinOp::Eq, [egraph.write(), self.chunk.permission]));
        self.condition.assert(egraph, perm_write).map_err(ExpressionError::write_missing)?;
        self.chunk.symbolic_value = symbolic_value;
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct HeapChunk {
    pub permission: egg::Id,
    pub symbolic_value: egg::Id,
}

impl Heap {
    fn get_resource(&mut self, egraph: &mut EGraph, resource: egg::Id, _condition: PathCondition) -> Result<&mut CTreeChunk, ExpressionError> {
        let resource = egraph.normalise(resource);
        // self.normalise(egraph);
        self.get_mut(&resource).ok_or(ExpressionError::resource_not_found(resource))
    }

    fn get_chunk(&mut self, egraph: &mut EGraph, resource: egg::Id, condition: PathCondition, skip: Option<Mutation>) -> Result<CTreeChunkUpdate, ExpressionError> {
        let ct_chunk = self.get_resource(egraph, resource, condition)?;
        Ok(ct_chunk.get_chunk(egraph, condition, skip))
    }

    pub fn get_symbolic_value(&mut self, egraph: &mut EGraph, resource: egg::Id, condition: PathCondition, skip: Option<Mutation>) -> Result<egg::Id, ExpressionError> {
        let chunk = self.get_chunk(egraph, resource, condition, skip)?;
        // println!("get_symbolic_value: {:#?}", self.0.get(&resource).unwrap());
        chunk.get_symbolic_value(egraph)
    }

    /// Returns `(value, permission)` but the value is only guaranteed to be
    /// valid if the permission is positive: it is up to the caller to check
    /// this. The `value` should only be used for snapshot purposes.
    pub fn get_permission(&mut self, egraph: &mut EGraph, resource: egg::Id, condition: PathCondition) -> Result<(egg::Id, egg::Id), ExpressionError> {
        let chunk = self.get_chunk(egraph, resource, condition, None)?;
        Ok((chunk.get_chunk_unsafe().symbolic_value, chunk.get_permission()))
    }

    pub fn update_symbolic_value(&mut self, egraph: &mut EGraph, resource: egg::Id, symbolic_value: egg::Id, condition: PathCondition) -> Result<(), ExpressionError> {
        let resource = self.get_resource(egraph, resource, condition)?;
        let update = resource.add_update(egraph, condition, None);
        update.update_value(egraph, symbolic_value)
    }

    /// Add `permission` amount to a heap chunk (identified by `resource`) under
    /// the given `condition`. If `bound` is `Some`, the permission will be
    /// assumed to be less than or equal to `bound`. A new symbolic value is
    /// created if we had no permission before. Returns the symbolic value of
    /// the chunk.
    pub fn add_permission(&mut self, egraph: &mut EGraph, resource: egg::Id, permission: egg::Id, condition: PathCondition, value: Option<egg::Id>, bound: Option<egg::Id>, config: PermissionConfig<()>) -> Result<egg::Id, ExpressionError> {
        let resource = egraph.normalise(resource);
        let resource = self.entry(resource).or_insert_with(|| CTreeChunk {
            chunk: HeapChunk {
                permission: egraph.none(),
                symbolic_value: value.unwrap_or_else(|| egraph.next_symbolic_value(None))
            },
            updates: Vec::new(),
        });
        if let PermissionConfig::Temporary { temporary: (false, temporary), .. } = config {
            let last = resource.updates.pop().unwrap();
            assert_eq!(last.temporary, Some(temporary));
            return Ok(last.chunk.symbolic_value)
        }
        let update = resource.add_update(egraph, condition, config.skip());
        update.add_permission(egraph, permission, value, bound, config.temporary())
    }

    /// Remove `permission` amount from a heap chunk (identified by `resource`)
    /// under the given `condition`. The symbolic value of the chunk is havoc'd
    /// if the permission becomes zero. Returns the old symbolic value before
    /// the update.
    pub fn remove_permission(&mut self, egraph: &mut EGraph, resource: egg::Id, permission: egg::Id, condition: PathCondition, config: PermissionConfig<Mutation>) -> Result<egg::Id, ExpressionError> {
        let resource = self.get_resource(egraph, resource, condition)?;
        if let PermissionConfig::Temporary { temporary: (false, temporary), .. } = config {
            let last = resource.updates.pop().unwrap();
            assert_eq!(last.temporary, Some(temporary));
            return Ok(last.chunk.symbolic_value)
        }
        let update = resource.add_update(egraph, condition, config.skip());
        update.remove_permission(egraph, permission, config.mutation(), config.temporary())
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
