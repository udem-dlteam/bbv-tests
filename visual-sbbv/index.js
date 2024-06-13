// General Display

const resizer = document.getElementById('resizer');
const controlPanel = document.getElementById('cfg-control-panel');
const cfgPanel = document.getElementById('cfg-display-panel');
const fileInput = document.getElementById('file-input');
const showAllSwitchElement = document.getElementById("cfg-show-all-switch-input");
const leftPanel = controlPanel;
const rightPanel = cfgPanel;
let isResizing = false;

resizer.addEventListener('mousedown', () => {
    isResizing = true;
    document.body.style.userSelect = "none"
    document.addEventListener('mousemove', panelHandleMouseMove);
    document.addEventListener('mouseup', panelStopResize);
});

function panelHandleMouseMove(e) {
    if (!isResizing) return;
    const leftWidth = e.clientX; // X position of mouse gives new left panel width
    leftPanel.style.width = `${leftWidth}px`; // Adjust the width of the left panel
    rightPanel.style.width = `calc(100% - ${leftWidth}px - 10px)`; // Adjust the right panel width
}

function panelStopResize() {
    isResizing = false;
    document.body.style.userSelect = "auto"
    document.removeEventListener('mousemove', panelHandleMouseMove);
    document.removeEventListener('mouseup', panelStopResize);
}

class UndoStack {
    constructor() {
        this.stack = [];
    }

    push(action) {
        this.stack.push(action);
    }

    undo() {
        let last = this.stack.pop();
        if (last) {
            last();
            return true;
        }
        return false;
    }

    undoAll() {
        while (this.undo());
    }
}

// Viz Panel
var activeNetworkId = "smallCFGElement";
const networkElements = {
    largeCFGNetwork: document.getElementById('full-cfg-element'),
    smallCFGNetwork: document.getElementById('small-cfg-element'),
    mergeHistoryNetwork: document.getElementById('merge-graph-element'),
}
const networks = Object.keys(networkElements).reduce((obj, key) => { obj[key] = null; return obj; }, {});

function getActiveNetwork() {
    return networks[activeNetworkId];
}

function getActiveNetworkElement() {
    return networkElements[activeNetworkId];
}

function destroyNetworks() {
    for (let key of Object.keys(networks)) {
        if (networks[key]) networks[key].destroy();
        networks[key] = null;
    }
    activeNetworkId = "smallCFGElement";
}

function getNetWorkUniqueId(block) {
    return `${block.bbs}-${block.id}`
}

var highlightUndos = new UndoStack();

function highlightBlock(block) {
    highlightUndos.undoAll();

    highlightBlockCard(block);
    highlightInCFGNetwork('largeCFGNetwork', block);
    highlightInCFGNetwork('smallCFGNetwork', block);
    highlightInMergeHistory(block.id);
}

function highlightBlockCard(block) {
    let elementId = getHtmlIdLocation(block);

    let element = document.getElementById(elementId);

    if (!element) return;

    element.classList.add('glow');

    let elementRef = new WeakRef(element);
    highlightUndos.push(() => {
        let element = elementRef.deref();
        if (!element) return;
        element.classList.remove('glow');
    })
}

function highlightInMergeHistory(blockId) {
    let network = networks.mergeHistoryNetwork;
    if (!network) return;

    let updates = network.body.data.nodes.map(
        (node) => {
            return {
                id: node.id,
                shadow: {
                    enabled: true,
                    color: 'red', // Customize the glow color
                    size: 30,
                    x: 0,
                    y: 0
                }
            }
        },
        {
            filter: (node) => node.refs.includes(blockId)
        }
    )

    network.body.data.nodes.update(updates)
}

function highlightInCFGNetwork(networkId, block) {
    let network = networks[networkId];
    if (!network) return;
    let nodeId = getNetWorkUniqueId(block);

    network.body.data.nodes.update({
        id: nodeId,
        shadow: {
            enabled: true,
            color: 'red', // Customize the glow color
            size: 30,
            x: 0,
            y: 0
        }
    })

    const networkRef = new WeakRef(network);
    highlightUndos.push(() => {
        if (!networkRef.deref()) return;
        networkRef.deref().body.data.nodes.update({
            id: nodeId,
            shadow: { enabled: false }
        })
    })
}

function centerOnBlockInNetwork(nodeId) {
    if (activeNetworkId === "mergeHistoryNetwork") {
        let networkToReturnTo = showAllSwitchElement.checked ? "largeCFGNetwork" : "smallCFGNetwork";
        focusNetworkPanel(networkToReturnTo)
    }

    const activeCFGNetwork = getActiveNetwork();

    if (!activeCFGNetwork) return;

    const nodePosition = activeCFGNetwork.getPositions([nodeId])[nodeId];

    if (nodePosition === undefined) {
        // glowElement(document.getElementById("cfg-show-all-switch").querySelector(".slider"))
        return
    }

    // Move the view to center on the node
    activeCFGNetwork.moveTo({
        position: nodePosition,
        scale: 1,  // You can adjust the scale as needed
        animation: {
            // Optional: animate the transition
            duration: 1000,
            easingFunction: "easeInOutQuad"
        }
    });
}

function focusNetworkPanel(networkId) {
    for (let [id, el] of Object.entries(networkElements)) {
        if (networkId !== id) el.style.display = "none";
    }
    networkElements[networkId].style.display = "block";
    activeNetworkId = networkId;
}

function refreshGraph(cfg, networkID, showAll) {
    let element = networkElements[networkID]
    focusNetworkPanel(networkID);

    if (networks[networkID]) return;

    const isActive = (block) => block.usage > 0;

    // create an array with nodes
    var nodes = new vis.DataSet(cfg.specializedBlocks.flatMap(
        (block) => {
            if (!showAll && !isActive(block)) return  [];
            return {
                id: getNetWorkUniqueId(block),
                title: `bbs: ${block.bbs}, usage: ${block.usage}`,
                label: block.details,
                shape: "box",
                font: {
                    "face": "monospace",
                    "align": "left"
                },
                color: {
                    background: block.usage === 0 ? "#d6e4f2" : "#f7f7f7",
                    border: "#9db4ca",
                },
                block: block
            }
        }
    ));

    // create an array with edges
    var edges = new vis.DataSet(cfg.specializedBlocks.flatMap(
        (from) => {
            return from.references.map(
                ({block, count, isReturnAddress, isReference, isStatic}) => {
                    let active = count > 0 || (isReturnAddress && from.usage > 0);

                    let description = `from: ${from.id} | to: ${block.id}\n`;
                    if (isReturnAddress) {
                        description += `return address\n`
                    }
                    if (isReference) {
                        description += `reference\n`
                    }
                    if (count !== undefined) {
                        description += `traversed: ${count}\n`
                    }

                    return {
                        from: getNetWorkUniqueId(from),
                        to: getNetWorkUniqueId(block),
                        arrows: "to",
                        color: active ? "#a0a0a0" : "#c0d0e0",
                        dashes: !isStatic,
                        label: count,
                        title: description,
                        selfReference: {
                            angle: 0,
                            size: 70,
                        },
                        smooth: {
                            enabled: true,
                            type: "cubicBezier",
                            forceDirection: "vertical",
                            roundness: 0.7,
                        }
                    }
                }
            )
        }
    ));

    // create a network
    var data = {
        nodes: nodes,
        edges: edges
    };
    var options = {
        layout: {
            improvedLayout: true,
            hierarchical: {
                enabled: true,
                nodeSpacing: 1000,
                blockShifting: false,
            },
        },
        physics: {
            enabled: false
        }
    };
    let activeCFGNetwork = new vis.Network(element, data, options);
    networks[networkID] = activeCFGNetwork;

    let lastFocus = null;
    let focusDirection = 'to';

    activeCFGNetwork.on("click", function (params) {
        if (params.nodes.length > 0) {
            let nodeId = params.nodes[0];
            let block = activeCFGNetwork.body.data.nodes.get(nodeId).block;
            scrollToBlock(block);
        } else if (params.edges.length > 0) {
            let edgeId = params.edges[0];
            let edge = activeCFGNetwork.body.data.edges.get(edgeId);

            let nextFocus = activeCFGNetwork.body.data.nodes.get(edge[focusDirection])

            if (lastFocus?.id === nextFocus.id) {
                focusDirection = focusDirection === "to" ? "from" : "to"
                nextFocus = activeCFGNetwork.body.data.nodes.get(edge[focusDirection])
            }
            lastFocus = nextFocus
            scrollToBlock(nextFocus.block)
        }
    });

    activeCFGNetwork.on("doubleClick", function (params) {
        if (params.nodes.length > 0) {
            let nodeId = params.nodes[0];
            let specializedBlock = activeCFGNetwork.body.data.nodes.get(nodeId).block;
            showHistory(specializedBlock);
        }
    })
}

function buildHistory(originBlock) {
    let history = filterRedundantReachability(originBlock.history);
    let currentLayer = [];
    let layers = [currentLayer]
    let edges = [];

    let order = 1;
    function addNode(node, froms, noOrderLabel) {
        if (froms === undefined) froms = [];
        else if (froms.constructor !== Array) froms = [froms];

        node = { ...node, id: newId() }

        let orderText = noOrderLabel ? "" : `(${order}) `;
        order += 1;
        froms.forEach(from => edges.push({ from, to: node.id }));
        if (currentLayer.find(node => froms.includes(node.id))) addLayer();
        currentLayer.push({ ...node, label: `${orderText}${node.label}`, level: layers.length})
    }

    function filterRedundantReachability(history) {
        live = {}
        return history.filter((event) => {
            switch (event.event) {
                case "create":
                    live[event.id] = true;
                    return true;
                case "mergeCreate":
                case "merge":
                    live[event.id] = true;
                    event.merged.forEach(id => live[id] = false);
                    return true;
                case "reachable":
                    let keepReachable = !live[event.id];
                    live[event.id] = true;
                    return keepReachable;
                case "unreachable":
                    let keepUnreachable = live[event.id];
                    live[event.id] = false;
                    return keepUnreachable;
                default:
                    return true;
            }
        })
    }

    function isLive(specializedBlockId) {
        for (let layer of layers.concat([currentLayer]).reverse()) {
            for (let node of layer) {
                if (node.keep === specializedBlockId) return true;
                if (node.kill.includes(specializedBlockId)) return false;
            }
        }
        return false;
    }

    function addLayer() {
        if (currentLayer.length === 0) return;
        currentLayer = [];
        layers.push(currentLayer);
    }

    function finalizeHistory() {
        addLayer();
        for (let specializedBlock of originBlock.versions) {
            addNode({
                label: `Final #${specializedBlock.id}\n${specializedBlock.context}`,
                keep: specializedBlock.id,
                kill: [],
            }, idOfLastReferenceTo(specializedBlock.id), true)
        }
        addLayer();
    }

    function positionOfLastReferenceTo(specializedBlockId) {
        for (let layerIndex = layers.length - 1; layerIndex >= 0; layerIndex--) {
            let layer = layers[layerIndex];
            for (let nodeIndex = 0; nodeIndex < layer.length; nodeIndex++) {
                let node = layer[nodeIndex];
                if (node.keep === specializedBlockId || node.kill.includes(specializedBlockId)) {
                    return { layerIndex, nodeIndex }
                }
            }
        }

        return { layerIndex: null, nodeIndex: null }
    }

    function lastReferenceTo(specializedBlockId) {
        let { layerIndex, nodeIndex } = positionOfLastReferenceTo(specializedBlockId);
        return layers[layerIndex][nodeIndex];
    }

    const idOfLastReferenceTo = (specializedBlockId) => lastReferenceTo(specializedBlockId).id;

    let _id = 0;
    const newId = () => _id++;
    history.forEach((event) => {
        switch (event.event) {
            case "create":
                addNode({
                    label: `#${event.id} Create from #${event.from}:\n${event.context}`,
                    keep: event.id,
                    kill: [],
                })
                break;
            case "mergeCreate":
            case "merge":
                addNode({
                    label: `Merge #${event.id} ← ${event.merged.map(x => `#${x}`).join(", ")}:\n${event.context}`,
                    keep: event.id,
                    kill: event.merged.filter(id => id !== event.id),
                }, event.merged.map(idOfLastReferenceTo));
                break;
            case "unreachable":
                if (isLive(event.id)) {
                    addNode({
                        label: `Unreachable #${event.id}`,
                        keep: null,
                        kill: [event.id],
                    }, idOfLastReferenceTo(event.id))
                }
                break;
            case "reachable":
                if (!isLive(event.id)) {
                    addNode({
                        label: `Reachable #${event.id}`,
                        keep: event.id,
                        kill: [],
                    }, idOfLastReferenceTo(event.id))
                }
                break;
            default:
                throw new Error("unknow event kind");
        }
    })

    finalizeHistory();

    console.log({
        edges,
        nodes: layers.flatMap((layer, index) => {
            return layer.map((node) => ({
                level: index,
                refs: node.kill.concat(node.keep ? [node.keep] : []),
                ...node,
            }))
        })
    })

    return {
        edges,
        nodes: layers.flatMap((layer, index) => {
            return layer.map((node) => ({
                level: index,
                refs: node.kill.concat(node.keep ? [node.keep] : []),
                ...node,
            }))
        })}
}

function showHistoryFromId(bbs, id) {
    showHistory({ id, bbs })
}

function showHistory(specializedBlock) {
    let { id, bbs } = specializedBlock;
    let originBlock = SBBVControlFlowGraph.getOriginBlockOfHistoricVersion(bbs, id)

    let { nodes, edges } = buildHistory(originBlock);

    let edgeOptions = {
        arrows: "to",
        color: "#a0a0a0",
        selfReference: {
            angle: 0,
            size: 70,
        }
    }

    let nodeOptions = {
        shape: "box",
        font: {
            "face": "monospace",
            "align": "left"
        },
        color: {
            background: "#f7f7f7",
            border: "#9db4ca",
        }
    }

    // create a network
    var data = {
        nodes: new vis.DataSet(nodes),
        edges: new vis.DataSet(edges),
    };
    
    var options = {
        edges: edgeOptions,
        nodes: nodeOptions,
        layout: {
            hierarchical: {
                enabled: true,
                levelSeparation: originBlock.cfg.compiler === "gambit" ? 400 : 800,
                blockShifting: false,
                edgeMinimization: true,
                sortMethod: "directed",
                direction: "LR",
                shakeTowards: "leaves",
                parentCentralization: false
            },
        },
        physics: {
            enabled: true,
            solver: 'barnesHut',
            hierarchicalRepulsion: {
                centralGravity: 1.1,
                springLength: 100,
                springConstant: 0.01,
                nodeDistance: 50,
                damping: 0.2,
                avoidOverlap: 0.9
            },
        }
    };

    let networkId = "mergeHistoryNetwork";
    focusNetworkPanel(networkId)
    let element = getActiveNetworkElement();
    networks[networkId] = new vis.Network(element, data, options);

    highlightInMergeHistory(id);
}

// Control Flow Graph

SBBVControlFlowGraph = null;

class SpecializedCFG {
    #specializedBlocks
    #originBlocks
    #historyBlocksToOrigin
    #historyBlocksOfOrigin

    constructor(compiler) {
        this.compiler = compiler
        this.#specializedBlocks = {}
        this.#originBlocks = {}
        this.#historyBlocksToOrigin = {}
        this.#historyBlocksOfOrigin = {}
    }

    cleanupDeadBBS() {
        if (this.compiler === "bigloo") return;

        let liveBBS = {};

        this.specializedBlocks.forEach(block => { if (block.usage > 0) liveBBS[block.bbs] = true; })

        let newSpecializedBlocks = {}
        let newOriginBlocks = {}

        for (let [bbs, blocks] of Object.entries(this.#specializedBlocks)) {
            for (let [id, block] of Object.entries(blocks)) {
                if (liveBBS[bbs]) {
                    if (!newSpecializedBlocks[bbs]) { newSpecializedBlocks[bbs] = {} };
                    newSpecializedBlocks[bbs][id] = block
                }
            }
        }

        for (let [bbs, blocks] of Object.entries(this.#originBlocks)) {
            for (let [id, block] of Object.entries(blocks)) {
                if (liveBBS[bbs]) {
                    if (!newOriginBlocks[bbs]) { newOriginBlocks[bbs] = {} };
                    newOriginBlocks[bbs][id] = block
                }
            }
        }

        this.#specializedBlocks = newSpecializedBlocks
        this.#originBlocks = newOriginBlocks
    }

    #getOrSetOriginBlock({ bbs, origin, source }) {
        let id = origin;
        if (!this.#originBlocks[bbs]) {
            this.#originBlocks[bbs] = {}
        }

        if (!this.#originBlocks[bbs][id]) {
            this.#originBlocks[bbs][id] = new OriginBasicBlock(id, bbs, source, this)
        }

        return this.#originBlocks[bbs][id]
    }

    getSpecializedBlock(bbs, id) {
        return this.#specializedBlocks[bbs]?.[id]
    }

    getOriginBlock(bbs, id) {
        return this.#originBlocks[bbs]?.[id]
    }

    getOriginBlockOfHistoricVersion(bbs, id) {
        return this.getOriginBlock(bbs, this.#historyBlocksToOrigin[bbs][id])
    }

    getHistoricVersionsOfOriginBlock(block) {
        return [...this.#historyBlocksOfOrigin[block.bbs][block.id]]
    }

    addBlock(block) {
        let { id, bbs } = block;
        let originBlock = this.#getOrSetOriginBlock(block);
        let specializedBlock = new SpecializedBasicBlock({...block, originBlock, cfg: this})

        originBlock.versions.push(specializedBlock)

        if (!this.#specializedBlocks[bbs]) {
            this.#specializedBlocks[bbs] = {}
        }

        this.#specializedBlocks[bbs][id] = specializedBlock
    }

    get originBlocks() {
        let blocks = [];
        for (let bbs of Object.values(this.#originBlocks)) {
            for (let bb of Object.values(bbs)) {
                blocks.push(bb)
            }
        }
        return blocks
    }

    get specializedBlocks() {
        let blocks = [];
        for (let bbs of Object.values(this.#specializedBlocks)) {
            for (let bb of Object.values(bbs)) {
                blocks.push(bb)
            }
        }
        return blocks
    }

    loadHistory(history) {
        let toOriginMapping = this.#historyBlocksToOrigin
        let fromOriginMapping = this.#historyBlocksOfOrigin
        for (let event of history) {
            let { origin, bbs, id } = event

            if (!toOriginMapping[bbs]) toOriginMapping[bbs] = {};
            if (!fromOriginMapping[bbs]) fromOriginMapping[bbs] = {};
            if (!fromOriginMapping[bbs][origin]) fromOriginMapping[bbs][origin] = new Set();

            toOriginMapping[bbs][id] = origin;
            fromOriginMapping[bbs][origin].add(id);
        }
    }
}

class OriginBasicBlock {
    constructor(id, bbs, source, cfg) {
        this.id = id
        this.bbs = bbs
        this.source = source
        this.cfg = cfg
        this.versions = []
        this.history = []
    }

    totalUsage() {
        let sum = 0;
        for (let b of this.versions) {
            sum = sum + b.usage
        }
        return sum
    }

    get loc() {
        return this.versions[0].loc
    }
}

class SpecializedBasicBlock {
    #predecessors
    #successors
    #jumps
    #references
    #ret

    constructor({ id, originBlock, context, loc, details, usage, predecessors, successors, ret, references, jumps, cfg }) {
        this.id = id
        this.originBlock = originBlock
        this.context = context
        this.details = details
        this.usage = usage
        this.#predecessors = predecessors
        this.#successors = successors
        this.#references = references || []
        this.#jumps = jumps || [];
        this.#ret = ret || []
        this.cfg = cfg
        this.loc = loc
    }

    get bbs() {
        return this.originBlock.bbs
    }

    get predecessors() {
        return this.#predecessors.map((p) => this.cfg.getSpecializedBlock(this.bbs, p)).filter(x => x)
    }

    get successors() {
        return this.#successors.map((p) => this.cfg.getSpecializedBlock(this.bbs, p)).filter(x => x)
    }

    get references() {
        let refs = {}
        let bbs = this.bbs

        const addRef = (bbs, id, data) => {
            let exists = !!refs[bbs] && !!refs[bbs][id];
            if (!refs[bbs]) refs[bbs] = {};
            if (!refs[bbs][id]) refs[bbs][id] = {};


            if (typeof data === 'function') {
                refs[bbs][id] = { ...(data(exists)), ...refs[bbs][id] }
            } else {
                refs[bbs][id] = { ...data, ...refs[bbs][id] }
            }
        }

        this.#successors.forEach(id => addRef(bbs, id, { isStatic: true }))
        this.#ret.forEach(id => addRef(bbs, id, { isReturnAdress: true }))
        this.#jumps.forEach(({ id, bbs, count }) => addRef(bbs, id, { count }))
        this.#references.forEach(id => addRef(bbs, id, (exists) => ({ isReference: !exists })))

        return Object.entries(refs).flatMap(
            ([bbs, bbsRefs]) => Object.entries(bbsRefs).map(
                ([id, data]) => {
                    return {
                        block: this.cfg.getSpecializedBlock(bbs, id),
                        ...data
                    }
                }
            )
        )
    }
}

function parseCFG({ compiler, specializedCFG }) {
    let cfg = new SpecializedCFG(compiler);

    for (let block of specializedCFG) {
        cfg.addBlock(block)
    }

    cfg.cleanupDeadBBS()

    return cfg
}

function parseHistory({ history }, cfg) {
    cfg.loadHistory(history);
    for (let event of history) {
        let { origin, bbs } = event
        let block = cfg.getOriginBlock(bbs, origin)
        if (block) block.history.push(event); // block may not exist if bbs is dead code
    }
}

// IO

function getHtmlIdLocation(block) {
    if (block instanceof SpecializedBasicBlock) {
        return `specialized-block-${block.bbs}-${block.id}`
    } else if (block instanceof OriginBasicBlock) {
        return `origin-block-${block.bbs}-${block.id}`
    }
    throw new Error("not a block", block)
}

function scrollToBlockById(bbs, specializedBlockId) {
    scrollToBlock(SBBVControlFlowGraph.getSpecializedBlock(bbs, specializedBlockId))
}

function scrollToBlock(block) {
    let originHTMLId = getHtmlIdLocation(block.originBlock)
    let nodeNetworkId = getNetWorkUniqueId(block);

    let card = document.getElementById(originHTMLId);
    openCard(card);
    card.scrollIntoView({ behavior: "smooth", block: "center", inline: 'nearest' });
    centerOnBlockInNetwork(nodeNetworkId);
    highlightBlock(block);
}

function escapeHtml(unsafe) {
    return unsafe
        .replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
        .replace(/"/g, "&quot;")
    //.replace(/'/g, "&#039;")
}

function toggleCard(cardElement) {
    const body = cardElement.querySelector('.origin-block-card-body');
    body.style.display = body.style.display === 'block' ? 'none' : 'block';
}

function openCard(cardElement) {
    const body = cardElement.querySelector('.origin-block-card-body');
    if (body.style.display !== 'block') body.style.display = 'block';
}

function closeCardBody(body) {
    if (body.style.display === 'block') body.style.display = 'none';
}

function closeCard(cardElement) {
    const body = cardElement.querySelector('.origin-block-card-body');
    closeCardBody(body);
}

function closeAllCards() {
    const bodies = document.querySelectorAll('.origin-block-card-body');
    bodies.forEach(closeCardBody)
}

function linkBlockRef(originBlock, code, refStyle) {
    let cfg = originBlock.cfg
    let compiler = refStyle || cfg.compiler
    let pattern

    if (compiler === "gambit") {
        pattern = /#(\d+)/g
    } else if (compiler === "bigloo") {
        pattern = /\(go\s+(\d+)\)/g
    } else {
        throw new Error("unknown compiler", compiler)
    }

    return code.replace(pattern, (match, number) => {
        let specializedBlock = cfg.getSpecializedBlock(originBlock.bbs, parseInt(number))
        if (!specializedBlock) return match; // may have mismatched a non-label

        let tooltip = `usage: ${specializedBlock.usage}`
        let classes = ["bb-ref-button"]
        if (specializedBlock.usage === 0) classes.push("low-importance-button")
        
        return `<button class="${classes.join(" ")}" data-tooltip="${tooltip}" onclick="scrollToBlockById('${specializedBlock.bbs}', '${specializedBlock.id}')">${match}</button>`;
    })
}

function handleShowAllSwitch() {
    if (!SBBVControlFlowGraph) return;
    let showAll = showAllSwitchElement.checked;
    refreshGraph(SBBVControlFlowGraph, showAll ? "largeCFGNetwork" : "smallCFGNetwork", showAll)
}

function abbreviateNumber(num) {
    if (num < 1000) {
        return num.toString();
    }
    else if (num < 1000000) {
        return (num / 1000).toFixed(0) + 'k';
    }
    else if (num < 1000000000) {
        return (num / 1000000).toFixed(0) + 'M';
    }
    else {
        return '1G+';
    }
}

function refreshControlPanel(cfg) {
    let newControlPanelHTML = ""

    const blocks = cfg.originBlocks;
    blocks.sort((b1, b2) => b2.totalUsage() - b1.totalUsage())

    for (let block of blocks) {
        let versions = [...block.versions]
        versions.sort((b1, b2) => b2.usage - b1.usage)

        newControlPanelHTML += `
        <div id="${getHtmlIdLocation(block)}" class="origin-block-card">
            <div class="origin-block-card-header">
                <span class="origin-block-card-title">
                    ${block.bbs}#${block.id}
                </span>
                <span class="origin-block-card-subtitles">
                    ${versions.map((b) => `<span class="origin-block-card-subtitle">${linkBlockRef(b, "#" + b.id, "gambit")}: ${abbreviateNumber(b.usage)}</span>`).join("")}
                </span>
            </div>
            <div class="origin-block-card-body">
                ${block.loc ? "<code>" + block.loc + "</code>" : ""}
                ${block.source ? "<h4>source</h4><code>" + escapeHtml(block.source) + "</code>" : ""}
                ${versions.map((b) => {
                    return `
                        <span id="${getHtmlIdLocation(b)}" class="origin-block-card-body-row">
                            <h4>Block ${linkBlockRef(block, "#" + b.id, "gambit")} (usage: ${abbreviateNumber(b.usage)})</h4>
                            <h5>Context</h5>
                            <code>${b.context}</code>
                            <h5>Predecessors</h5>
                            ${b.predecessors.length > 0 ? linkBlockRef(block, b.predecessors.map((p) => "#" + p.id).join(", "), "gambit") : "-"}
                            <h5>Successors</h5>
                            ${b.successors.length > 0 ? linkBlockRef(block, b.successors.map((p) => "#" + p.id).join(", "), "gambit") : "-"}
                            <h5>Code</h5>
                            <code>${b.details ? linkBlockRef(block, escapeHtml(b.details)) : linkBlockRef(block, escapeHtml(b.context))}</code>
                        </span>`
        }).join("")}
            <h4>History</h4>
            ${cfg.getHistoricVersionsOfOriginBlock(block).map((id) => {
                let classes = ["bb-ref-button", "history-ref-button"]
                if (!cfg.getSpecializedBlock(block.bbs, id)) classes.push("low-importance-button")
                return `<button class='${classes.join(" ") }'
                                onclick="showHistoryFromId('${block.bbs}', ${id})">#${id}</button>`
            }).join(" ")}
            </div>
        </div>
        `
    }

    controlPanel.innerHTML = newControlPanelHTML

    const cards = document.querySelectorAll('.origin-block-card');

    cards.forEach(card => {
        card.querySelector('.origin-block-card-header').addEventListener('click', () => toggleCard(card));
    })
}

function loadJSON(content) {
    try {
        var json = JSON.parse(content)
    } catch (error) {
        alert(error)
        return;
    }
    SBBVControlFlowGraph = parseCFG(json)
    parseHistory(json, SBBVControlFlowGraph)
    refreshControlPanel(SBBVControlFlowGraph)
    refreshGraph(SBBVControlFlowGraph, "smallCFGNetwork", false)
}

function loadCFGFromFile(file) {
    destroyNetworks();
    showAllSwitchElement.checked = false;
    var reader = new FileReader()
    reader.onload = e => loadJSON(e.target.result)
    reader.readAsText(file)
}

if (fileInput.files[0]) loadCFGFromFile(fileInput.files[0]);

fileInput.addEventListener('change', function (event) {
    var file = event.target.files[0]
    if (file) loadCFGFromFile(file);
});
