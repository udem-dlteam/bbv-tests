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

# Things which are optimizations and should be removed from the core algo:
# 1) all_versions
# 2) GC scheduling

# Things to improve: merge as late as possible, more information is better


def BBV(source : CFG,
        initial_context: Context,
        VERSION_LIMIT: int,
        MERGE_HEURISTIC: Function[List[Version], List[Version]]):

    work_queue = []

    root_version = reach(source.entry, initial_context, work_queue, VERSION_LIMIT, MERGE_HEURISTIC)

    while len(work_queue) > 0:
        version = work_queue.pop()
        walk(version, work_queue, VERSION_LIMIT, MERGE_HEURISTIC)
        GC() # Can be optimized and skiped sometimes

    return live_version(root_version)

def reach(block: BasicBlock,
          ctx: Context,
          work_queue: Queue,
          VERSION_LIMIT: int,
          MERGE_HEURISTIC: Function[List[Version], List[Version]]) -> Version:

    if ctx in block.all_versions:
        # The version already exists
        version = live_version(block.all_versions[ctx])
    else:
        # The version did not exist
        version = new_version(block, ctx)

    if version in block.live_versions:
        return version

    block.all_versions.add(version)
    block.live_versions.add(version)

    if len(block.live_versions) >= VERSION_LIMIT:
        # Too many versions, merge
        return merge(block, work_queue, VERSION_LIMIT, MERGE_HEURISTIC)
    else:
        # Schedule traversal of the new found version
        work_queue.add(version)
        return version


def merge(block: BasicBlock,
          work_queue: Queue,
          VERSION_LIMIT: int,
          MERGE_HEURISTIC: Function[List[Version], List[Version]]):

    # Too many version, identify versions to merge together
    versions_to_merge = MERGE_HEURISTIC(block.live_versions)

    # Remove the versions to merge
    for v in versions_to_merge:
        block.live_versions.remove(v)

    # Merge the versions by generating a new context from the union of their contexts
    ctxs = [version.context_before for version in versions_to_merge]
    new_context = union_with_widening(ctxs)

    version = new_version(block, new_context)

    # Mark versions as previously merged, for aliasing
    for v in versions_to_merge:
        v.merge = version

    block.all_versions.add(version)
    block.live_versions.add(version)

    if version not in versions_to_merge:  # TODO: I think Gambit does not do that check
        # Schedule the newly created version for traversal
        work_queue.add(version)

    return version


def walk(version: Version,
         work_queue: Queue,
         VERSION_LIMIT: int,
         MERGE_HEURISTIC: Function[List[Version], List[Version]]):

    block = version.block
    context_before = version.context_before

    specialized_instructions = []
    context_after = context_before

    # Specialize each instruction with the context information of that version
    for instr in block.instructions:
        context_after, specialized_instruction = specialize_instruction(context_after, instr)

        if specialized_instruction:
            specialized_instructions.append(specialized_instruction)

    version.instructions = specialized_instructions

    # Specialize the branch instruction, in case of a conditional jump we also may gain
    # some contextual information for the target
    specialized_branch_instruction = specialize_branch_instruction(context_after, block.branch_instruction)

    version.branch_instruction = specialized_branch_instruction

    for context_after, target_block in specialized_branch_instruction:
        target_version = reach(target_block, context_after, work_queue, VERSION_LIMIT, MERGE_HEURISTIC)
        specialized_branch_instruction[target_block].version = target_version

# Version manipulation

def new_version(block: BasicBlock,
                ctx: Context):
    return Version(block=block,
                   context_before=ctx,
                   instructions=None,
                   branch_instruction=None,
                   merge=None)


def live_version(version: Version):
    if version.merge:
        return live_version(version.merge)
    else:
        return version

# GC function
def GC():
    unschedule_GC()
    mark_all_versions_as_unreachable()
    traverse_versions_from_the_root_and_mark_them_as_reachable()
    remove_tasks_on_unreachable_versions_from_the_queue()
