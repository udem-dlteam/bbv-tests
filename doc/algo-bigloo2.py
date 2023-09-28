"""
A CFG is a graph of BB, each with a label that identifies it (a pointer)
with an entry point called CFG.entry .

A BB is an array of instructions. BB.length is the number of instructions.
BB.instr[j] is the j^th instruction of BB.

BB.instr[j].live is the set of live variables before instruction j.

BB.ctx is the context used to generate BB.

typeof(v) is the result of a preceeding type analysis, or user declaration,
or simply "any".

The last instruction of a BB is a branch which can transfer control to any of
a set of successor BBs, denoted successors(BB).

reachable(lbls:set of labels) is the set of labels in lbls reachable from BB[1].

BBV is a function from a CFG to a CFG:

BBV(source:CFG, ctx, VERSION_LIMIT:int) => CFG
"""

def BBV(source, ctx0, VERSION_LIMIT):
    wq = []
    bs = new_version(source.entry, ctx0, wq)

    while wq.length > 0:
        bs = wq.pop()
        bv = bs.orig

        if need_merge(bv, VERSION_LIMIT):
            block_merge(bv, wq)

        if not block_merged(bs):
            block_specialize(bs, wq)
            
    return fs

def new_version(bv, ctx, wq):
    if ctx in bv.versions:
        return bv.versions[ctx]
    else:
        bs = new_BS()
    
        bv.versions[ctx] = bs
        bs.orig = bv
        bs.ctx = ctx
        wq.push(bs)

        return bs
    
def need_merge(bv, limit):
    return bv.versions >= limit

def block_merged(bs):
    return bs.merge = null

def block_merge(bv, wq):
    bs1, bs2 = pick 2 BBs from bv.versions
    ctx = union_with_widening(bs1.ctx, bs2.ctx)

    mbs = new_version(bv, ctx)

    if bs1 is mbs:
        mark_merged(bs2)
    else if bs2 is mbs:
        mark_merged(bs1)
    else:
        mark_merged(bs1, mbs)
        mark_merged(bs2, mbs)
        wq.push(mbs)

def mark_merged(obs, nbs):
    obs.merge = nbs;
    # modify the preds and succs so that everyone points to mbs 
    replace_block(obs, mbs)
    
def block_specialize(bs, wq):
    bv = bs.orig
    ctx = bs.ctx

    bs.instr = bv.instr.map(i => ins_specialize(i, ctx))


def ins_specialize(ins, ctx):
    if ins is ins_goto:
        return ins_specialize_goto(ins, ctx)
    else:
        ...

def ins_specialize_goto(ins, ctx):
    nins = ins.dup();
    nins.target = new_version(ins.target, ctx)
    return nins
