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

BBV( source:CFG, VERSION_LIMIT:int ) => CFG
"""



def BBV( source, VERSION_LIMIT ):

    wq = []
    versions[*] = {}

    def specialize(new_BB, ctx):
        orig_BB = new_BB.orig
        new_instrs = []
        new_ctx = ctx
        ...the obvious thing that changes new_ctx on the instructions orig_BB.instrs
        for each s in successors(orig_BB):
            patch successor s in new_BB with BBV_BB(s, new_ctx modulo narrowing)
        new_BB.instrs = new_instrs

    def BBV_BB( orig_BB, ctx ):

        if ctx in orig_BB.versions:
            return orig_BB.versions[ctx].representative

        new_BB = alloc_BB()
        orig_BB.versions[ctx] = new_BB
        new_BB.orig = orig_BB
        new_BB.ctx = ctx

        if reachable(orig_BB.versions).length >= VERSION_LIMIT:

            new_BB.instrs = []
            wq.push(new_BB)

        else:

            specialize(new_BB, ctx)

        return new_BB

    def merge( orig_BB ):
        while reachable(orig_BB.versions).length > VERSION_LIMIT:

            bb1, bb2, ctx = merge2(reachable(orig_BB.versions))

            if ctx in orig_BB.versions:
                new_BB = orig_BB.versions[ctx].representative
                replace branches to bb1 and bb2 with branches to new_BB
                continue

            new_BB = alloc_BB()
            orig_BB.versions[ctx] = new_BB
            new_BB.orig = orig_BB
            new_BB.ctx = ctx

            specialize(new_BB, ctx)


    def merge2(versions):
        bb1, bb2 = pick 2 BBs from versions
        ctx = union_with_widening(bb1.ctx, bb2.ctx)
        return bb1, bb2, ctx


    new_entry = BBV_BB( source.entry, { v:typeof(v) | v <- source.entry.live } )

    while wq not empty:
        new_BB = wq.pop()
        merge( new_BB.orig )

    return new_entry
