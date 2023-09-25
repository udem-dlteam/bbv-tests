"""
A CFG is a graph of BB, each with a label that identifies it (a pointer)
with an entry point called CFG.entry .

A BB is an node with an array of instructions bb.instrs and a last instruction
bb.last_instr which can either be a jump to a another bb, a jumpif which
conditionally jump to other bbs or some other form of jump which we do not traverse

Each BB has BBVersions which are versions of that bb. A BBVersion has a ctx_before
which is the context (types) at the entry of that BB and a bb, which is the bb it refers to

reachable(lbls:set of labels) is the set of labels in lbls reachable from BB[1].

BBV is a function from a CFG to a CFG:

BBV( source:CFG, VERSION_LIMIT:int ) => CFG
"""



def BBV(source, VERSION_LIMIT):

    work_queue = []

    def merged_ctx_mapping(bb, ctx):
        return result of mapping built in merge (recursively follows the mapping)

    def merge(orig_bb):
        # This could merge more than 2 versions, but we only do 2 right now
        versions_to_merge, versions_to_keep = choose_candidates(orig_bb.versions)
    
        new_versions = versions_to_keep

        merged_ctx = union_with_widening(versions_to_merge)

        if merged_ctx in versions_to_merge:
            # A version to merge was a superset of all others
            new_version = versions_to_merge[merged_ctx]
        else:
            new_version = BBVersion(ctx_before=merged_ctx, bb=orib_bb)
            work_queue.push(new_version)

        for ctx, version in versions_to_merge:
            merged_ctx_mapping(bb, ctx) -> merged_ctx # to compute representative
            version.merged_to = new_version

        new_versions[merged_ctx] = new_version

        orig_bb.versions = new_versions

        # GC to compute reachability and remove unreachables
        compute_reachability()


    def reach(orig_bb, ctx):
        ctx = orig_bb.all_seen_versions[ctx].representative

        if ctx in orig_bb.versions:
            return orig_bb.versions[ctx]

        new_version = BBVersion(ctx_before=ctx, bb=orib_bb)
        orig_bb.all_seen_versions[ctx] = orig_bb.versions[ctx] = new_version
        work_queue.push(new_version)

        if len(orig_bb.versions) > VERSION_LIMIT:
            merge(orig_bb)

        return new_version
        
    def walk_version(version): # Called walk_bb in Gambit
        bb = version.bb
        ctx = version.ctx_before

        for instr in version.bb.instrs:
            ctx = symbolic_execution(ctx, instr)

        if bb.last_instruction is jump:
            version.target = reach(bb.target, ctx)
        elif bb.last_instruction is jumpif:
            version.target_if_true = reach(bb.target_if_true, ctx_if_true)
            version.target_if_false = reach(bb.target_if_false, ctx_if_false)

    new_entry = reach(source.entry, empty_context)
    
    while work_queue is not empty:
        version = work_queue.pop()
        if reachable(version):
            walk_version(version)

    rearrange CFG by replacing all bbversions with their bbversion.merged_to representative recursively

    return new_entry
