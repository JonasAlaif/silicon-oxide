use std::ops::{Deref, DerefMut};

use fxhash::FxHashMap;

use crate::{exp::{BinOp, Exp}, pure::EGraph};

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

#[derive(Debug, Clone)]
pub struct CTreeChunkUpdate {
    pub condition: egg::Id,
    pub neg_condition: egg::Id,
    pub ct_chunk: CTreeChunk,
}

impl CTreeChunk {
    pub fn get_chunk(&self, egraph: &mut EGraph, neg_pc: egg::Id, pos_perm: bool) -> Result<HeapChunk, ()> {
        fn return_(egraph: &mut EGraph, chunks: Vec<(egg::Id, HeapChunk)>, chunk: HeapChunk) -> HeapChunk {
            chunks.into_iter().fold(chunk, |acc, (condition, chunk)| {
                let permission = egraph.add(Exp::Ternary([condition, chunk.permission, acc.permission]));
                let symbolic_value = egraph.add(Exp::Ternary([condition, chunk.symbolic_value, acc.symbolic_value]));
                HeapChunk { permission, symbolic_value }
            })
        }

        let mut neg_pc = neg_pc;
        let mut chunks: Vec<(egg::Id, HeapChunk)> = Vec::new();
        for ct_chunk in &self.updates {
            // println!("Checking update: {ct_chunk:?}");
            let def_false = egraph.add(Exp::BinOp(BinOp::Or, [neg_pc, ct_chunk.neg_condition]));
            let def_true = egraph.add(Exp::BinOp(BinOp::Or, [neg_pc, ct_chunk.condition]));
            egraph.saturate();

            if egraph.is_true(def_false) {
                // println!("Definitely false");
                // Could do `neg_pc = def_true;` but that should be equivalent
                // to just using `neg_pc` directly.
                continue;
            }

            let chunk = ct_chunk.ct_chunk.get_chunk(egraph, def_false, pos_perm)?;
            if egraph.is_true(def_true) {
                // println!("Definitely true");
                return Ok(return_(egraph, chunks, chunk));
            }
            // println!("Unknown");
            
            chunks.push((def_true, chunk));
            neg_pc = def_true;
        }

        let condition = egraph.add(Exp::BinOp(BinOp::Lt, [egraph.none(), self.chunk.permission]));
        let def_true = egraph.add(Exp::BinOp(BinOp::Or, [neg_pc, condition]));

        egraph.saturate();

        if egraph.is_true(def_true) {
            Ok(return_(egraph, chunks, self.chunk))
        } else if pos_perm {
            Err(())
        } else {
            let symbolic_value = egraph.next_symbolic_value(None);
            let symbolic_value = egraph.add(Exp::Ternary([def_true, self.chunk.symbolic_value, symbolic_value]));
            let chunk = HeapChunk {
                permission: self.chunk.permission,
                symbolic_value,
            };
            Ok(return_(egraph, chunks, chunk))
        }
    }

    /// The `neg_pc` should already include `neg_condition`.
    fn add_update(&mut self, egraph: &mut EGraph, neg_pc: egg::Id, condition: egg::Id, neg_condition: egg::Id) -> &mut CTreeChunk {
        let chunk = self.get_chunk(egraph, neg_pc, false).unwrap();
        let ct_chunk = CTreeChunk {
            chunk,
            updates: Vec::new(),
        };
        let update = CTreeChunkUpdate { condition, neg_condition, ct_chunk };
        self.updates.push(update);
        &mut self.updates.last_mut().unwrap().ct_chunk
    }

    pub fn add_pc(&mut self, egraph: &mut EGraph, pc: &[egg::Id]) -> (Option<egg::Id>, &mut CTreeChunk) {
        let mut curr = self;
        let mut neg_pc_curr = None;
        for condition in pc {
            let neg_condition = egraph.add(Exp::Not(*condition));
            let neg_pc = if let Some(neg_pc) = neg_pc_curr {
                egraph.add(Exp::BinOp(BinOp::Or, [neg_pc, neg_condition]))
            } else {
                neg_condition
            };
            curr = curr.add_update(egraph, neg_pc, *condition, neg_condition);
            neg_pc_curr = Some(neg_pc);
        }
        (neg_pc_curr, curr)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct HeapChunk {
    pub permission: egg::Id,
    pub symbolic_value: egg::Id,
}

impl Heap {
    fn get_chunk(&mut self, egraph: &mut EGraph, resource: egg::Id, pc: &[egg::Id], pos_perm: bool) -> Result<HeapChunk, ()> {
        let resource = egraph.normalise(resource);
        // TODO: this is not necessary to do every time
        // self.normalise(egraph);
        let ct_chunk = self.get(&resource).ok_or(())?;

        // TODO: this could just use `add_pc` directly (as does `update_symbolic_value`).
        let neg_pc = egraph.negate_pc(pc);
        ct_chunk.get_chunk(egraph, neg_pc, pos_perm)
    }

    pub fn get_symbolic_value(&mut self, egraph: &mut EGraph, resource: egg::Id, pc: &[egg::Id]) -> Result<egg::Id, ()> {
        Ok(self.get_chunk(egraph, resource, pc, true)?.symbolic_value)
    }

    pub fn update_symbolic_value(&mut self, egraph: &mut EGraph, resource: egg::Id, symbolic_value: egg::Id, pc: &[egg::Id]) -> Result<(), ()> {
        let resource = egraph.normalise(resource);
        let ct_chunk = self.get_mut(&resource).ok_or(())?;
        let (neg_pc, ct_chunk) = ct_chunk.add_pc(egraph, pc);
        let neg_pc = neg_pc.unwrap_or_else(|| egraph.false_());

        let mut chunk = ct_chunk.get_chunk(egraph, neg_pc, false).unwrap();

        let perm_write = egraph.add(Exp::BinOp(BinOp::Eq, [egraph.write(), chunk.permission]));
        let perm_write = egraph.add(Exp::BinOp(BinOp::Or, [neg_pc, perm_write]));
        egraph.saturate();
        if !egraph.is_true(perm_write) {
            return Err(());
        }

        chunk.symbolic_value = symbolic_value;

        ct_chunk.updates.clear();
        ct_chunk.chunk = chunk;
        Ok(())
    }

    pub fn get_permission(&mut self, egraph: &mut EGraph, resource: egg::Id, pc: &[egg::Id]) -> Result<egg::Id, ()> {
        Ok(self.get_chunk(egraph, resource, pc, false)?.permission)
    }

    pub fn add_chunk(&mut self, egraph: &mut EGraph, resource: egg::Id, permission: egg::Id, pc: &[egg::Id], bound: Option<egg::Id>) {
        let resource = egraph.normalise(resource);
        let ct_chunk = self.entry(resource).or_insert_with(|| {
            let chunk = HeapChunk {
                permission: egraph.none(),
                symbolic_value: egraph.next_symbolic_value(None),
            };
            CTreeChunk {
                chunk,
                updates: Vec::new(),
            }
        });

        let (neg_pc, ct_chunk) = ct_chunk.add_pc(egraph, pc);
        let neg_pc = neg_pc.unwrap_or_else(|| egraph.false_());
        let mut chunk = ct_chunk.get_chunk(egraph, neg_pc, false).unwrap();
        chunk.permission = egraph.add(Exp::BinOp(BinOp::Plus, [chunk.permission, permission]));

        ct_chunk.chunk = chunk;

        if let Some(bound) = bound {
            let bound = egraph.add_binop(silver_oxide::ast::BinOp::Le, chunk.permission, bound);
            let bound = egraph.add(Exp::BinOp(BinOp::Or, [neg_pc, bound]));

            egraph.assume(bound, "resource bound");
        }
    }

    pub fn remove_chunk(&mut self, egraph: &mut EGraph, resource: egg::Id, permission: egg::Id, pc: &[egg::Id]) -> Result<(), ()> {
        let resource = egraph.normalise(resource);
        let ct_chunk = self.get_mut(&resource).ok_or(())?;

        let (neg_pc, chunk) = ct_chunk.add_pc(egraph, pc);
        let permission = egraph.add_binop(silver_oxide::ast::BinOp::Minus, chunk.chunk.permission, permission);
        println!("Updated chunk {:?} -> {:?}", chunk.chunk.permission, permission);
        chunk.chunk.permission = permission;

        let neg_pc = neg_pc.unwrap_or_else(|| egraph.false_());

        let non_negative = egraph.add_binop(silver_oxide::ast::BinOp::Le, egraph.none(), chunk.chunk.permission);
        let non_negative = egraph.add(Exp::BinOp(BinOp::Or, [neg_pc, non_negative]));

        egraph.saturate();

        if egraph.is_true(non_negative) {
            Ok(())
        } else {
            Err(())
        }
    }
}
