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

# Things to improve: merge as late as possible, more information is better
# Do not be so specific about the fact that branch are last instr

walk_queue = []
merge_queue = []

def BBV(source : CFG,
        initial_context: Context,
        VERSION_LIMIT: int,
        MERGE_HEURISTIC: Function[List[Version], List[Version]]):

    root_version = reach(source.entry, initial_context, VERSION_LIMIT)

    while len(walk_queue) > 0 or len(merge_queue) > 0:
        while len(walk_queue) > 0:
            version = walk_queue.pop()
            walk(version, VERSION_LIMIT)

        while len(merge_queue) > 0:
            version = merge_queue.pop()
            merge(version, VERSION_LIMIT, MERGE_HEURISTIC)
            GC()

    return live_version(root_version)


def reach(block: BasicBlock,
          ctx: Context,
          VERSION_LIMIT: int) -> Version:
          
    if ctx in block.versions:
        return block.versions[ctx]
    
    version = new_version(block, ctx)

    block.versions.add(version)

    if len(block.versions) > VERSION_LIMIT:
        merge_queue.add(version)
    else:
        walk_queue.add(version)

    return version


def walk(version: Version,
         VERSION_LIMIT: int):

    block = version.block
    context_before = version.context_before

    for instr in block.instructions:
        specialized_instr, context_before = symbolic_execution(instr, context_before)
        version.instructions.append(specialized_instr)

    branch = block.last_instruction

    if branch.is_jump():
        new_branch = Jump(target=reach(branch.target, context_before, work_queue, VERSION_LIMIT))
    elif branch.is_conditional_jump():
        context_if_true, context_if_false = symbolic_execution_conditional(branch, context_before)
        
        if context_if_true is None: # Cannot be true
            new_branch = Jump(target=reach(branch.target_if_false, context_if_false, VERSION_LIMIT))
        elif context_if_false is None: # Cannot be false
            new_branch = Jump(target=reach(branch.target_if_true, context_if_true, VERSION_LIMIT))
        else: # Both branches still possible
            new_branch = ConditionalJump(
                target_if_true=reach(branch.target_if_true, context_if_true, VERSION_LIMIT),
                target_if_false=reach(branch.target_if_false, context_if_false, VERSION_LIMIT))

    version.last_instruction = new_branch


def merge(version: Version,
          VERSION_LIMIT: int,
          MERGE_HEURISTIC: Function[List[Version], List[Version]]):
    block = version.block

    # Too many version, identify versions to merge together
    versions_to_merge = MERGE_HEURISTIC(block.versions, VERSION_LIMIT)

    # Remove the versions to merge
    for v in versions_to_merge:
        block.live_versions.remove(v)

    # Merge the versions by generating a new context from the union of their contexts
    ctxs = [version.context_before for version in versions_to_merge]
    new_context = union_with_widening(ctxs)

    merged_version = new_version(block, new_context)

    # Mark versions as previously merged, for aliasing
    for v in versions_to_merge:
        v.merge = merged_version
        fix_references_to(v)

    block.versions.add(merged_version)

    if version in block.versions:
        # Version triggered a merge, but it was not merged so we now need to traverse it
        walk_queue.add(version)

    if merged_version not in versions_to_merge:
        # Schedule the newly created version for traversal
        walk_queue.add(version)


def fix_references_to(old_version):
    new_version = live_version(old_version)
    for predecessor in old_version.predecessors:
        predecessor.replace_references(old_version, new_version)


def new_version(block: BasicBlock,
                ctx: Context):
    # init an empty version
    return Version(block=block, context_before=ctx, instructions=[], last_instruction=None, merge=None)


def live_version(version: Version):
    while version.merge is not None:
        version = version.merge
    return version


def GC():
    if GC_is_required():
        mark_all_versions_as_unreachable()
        traverse_versions_from_the_root_and_mark_them_as_reachable()
        remove_tasks_on_unreachable_versions_from_the_queue()
