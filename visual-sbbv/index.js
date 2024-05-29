// General Display

const resizer = document.getElementById('resizer');
const controlPanel = document.getElementById('cfg-control-panel');
const cfgPanel = document.getElementById('cfg-display-panel');
const fileInput = document.getElementById('file-input');

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

// Viz Panel

cfgNetwork = null;

function getNetWorkUniqueId(block) {
    return `${block.bbs}-${block.id}`
}

function makeNodeGlow(nodeId) {
    cfgNetwork.body.data.nodes.update({
        id: nodeId,
        shadow: {
            enabled: true,
            color: 'red', // Customize the glow color
            size: 30,
            x: 0,
            y: 0
        }
    })

    setTimeout(() => {
        cfgNetwork.body.data.nodes.update({
            id: nodeId,
            shadow: { enabled: false }
        })
    }, 2000)
}

function centerOnBlockInNetwork(nodeId) {
    if (!cfgNetwork) return;

    const nodePosition = cfgNetwork.getPositions([nodeId])[nodeId];

    // Move the view to center on the node
    cfgNetwork.moveTo({
        position: nodePosition,
        scale: 1,  // You can adjust the scale as needed
        animation: {
            // Optional: animate the transition
            duration: 1000,
            easingFunction: "easeInOutQuad"
        }
    });

    makeNodeGlow(nodeId)
}

function refreshGraph(cfg) {
    if (cfgNetwork) cfgNetwork.destroy();

    // create an array with nodes
    var nodes = new vis.DataSet(cfg.specializedBlocks.map(
        (block) => {
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
            return from.jumps.map(
                ({block, count, hard, ret}) => {
                    let active = count > 0 || (ret && from.usage > 0);
                    return {
                        from: getNetWorkUniqueId(from),
                        to: getNetWorkUniqueId(block),
                        arrows: "to;middle",
                        color: active ? "#a0a0a0" : "#c0d0e0",
                        dashes: !hard || ret,
                        label: count,
                        title: ret ? `call from: ${from.id}\nreturn to: ${block.id}`
                                   : `from: ${from.id}\nto: ${block.id}\ntraversed: ${count}`,
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
    cfgNetwork = new vis.Network(cfgPanel, data, options);

    let lastFocus = null;
    let focusDirection = 'to';

    cfgNetwork.on("click", function (params) {
        if (params.nodes.length > 0) {
            let nodeId = params.nodes[0];
            let block = cfgNetwork.body.data.nodes.get(nodeId).block;
            scrollToBlock(block);
        } else if (params.edges.length > 0) {
            let edgeId = params.edges[0];
            let edge = cfgNetwork.body.data.edges.get(edgeId);

            let nextFocus = cfgNetwork.body.data.nodes.get(edge[focusDirection])

            if (lastFocus?.id === nextFocus.id) {
                focusDirection = focusDirection === "to" ? "from" : "to"
                nextFocus = cfgNetwork.body.data.nodes.get(edge[focusDirection])
            }
            lastFocus = nextFocus
            scrollToBlock(nextFocus.block)
        }
    });

}

// Control Flow Graph

SBBVControlFlowGraph = null;

class SpecializedCFG {
    #specializedBlocks
    #originBlocks

    constructor(compiler) {
        this.compiler = compiler
        this.#specializedBlocks = {}
        this.#originBlocks = {}
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

    #getOrSetOriginBlock(bbs, id, source) {
        if (!this.#originBlocks[bbs]) {
            this.#originBlocks[bbs] = {}
        }

        if (!this.#originBlocks[bbs][id]) {
            this.#originBlocks[bbs][id] = new OriginBasicBlock(id, bbs, source, this)
        }

        return this.#originBlocks[bbs][id]
    }

    getSpecializedBlock(bbs, id) {
        return this.#specializedBlocks?.[bbs][id]
    }

    getOriginBlock(bbs, id) {
        return this.#originBlocks?.[bbs][id]
    }

    addBlock({ id, bbs, origin, context, details, source, usage, predecessors, successors, ret, jumps }) {
        let originBlock = this.#getOrSetOriginBlock(bbs, origin, source);
        let specializedBlock = new SpecializedBasicBlock(id, originBlock, context, details, usage, predecessors, successors, ret, jumps, this)

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
}

class OriginBasicBlock {
    constructor(id, bbs, source, cfg) {
        this.id = id
        this.bbs = bbs
        this.source = source
        this.cfg = cfg
        this.versions = []
    }

    totalUsage() {
        let sum = 0;
        for (let b of this.versions) {
            sum = sum + b.usage
        }
        return sum
    }
}

class SpecializedBasicBlock {
    #predecessors
    #successors
    #jumps
    #ret

    constructor(id, originBlock, context, details, usage, predecessors, successors, ret, jumps, cfg) {
        this.id = id
        this.originBlock = originBlock
        this.context = context
        this.details = details
        this.usage = usage
        this.#predecessors = predecessors
        this.#successors = successors
        this.#ret = ret || []
        this.cfg = cfg

        this.#jumps = jumps || [];
        for (let succId of this.#successors) {
            if (!this.#jumps.find(({ bbs, id }) => bbs === this.bbs && id === succId)) {
                this.#jumps.push({ bbs: this.bbs, id: succId, count: 0 })
            }
        }
    }

    get bbs() {
        return this.originBlock.bbs
    }

    get predecessors() {
        return this.#predecessors.map((id) => this.cfg.getSpecializedBlock(this.bbs, id))
    }

    get successors() {
        return this.#successors.map((id) => this.cfg.getSpecializedBlock(this.bbs, id))
    }

    get jumps() {
        return [...this.#jumps.map(({bbs, id, count }) => {
            return {
                block: this.cfg.getSpecializedBlock(bbs, id),
                hard: bbs === this.bbs && this.#successors.includes(id),
                count,
            }
        }),
        ...this.#ret.map(id => {
            return {
                block: this.cfg.getSpecializedBlock(this.bbs, id),
                ret: true,
                count: 0,
            }
        })]
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

// IO

function getHtmlIdLocation(block) {
    if (block instanceof SpecializedBasicBlock) {
        return `specialized-block-${block.bbs}-${block.id}`
    } else if (block instanceof OriginBasicBlock) {
        return `origin-block-${block.bbs}-${block.id}`
    }
    throw new Error("not a block", block)
}

function scrollToBlock(specializedBlock) {
    let specializedId = getHtmlIdLocation(specializedBlock)
    let originId = getHtmlIdLocation(specializedBlock.originBlock)
    let nodeId = getNetWorkUniqueId(specializedBlock);
    scrollToBlockByIds(originId, specializedId, nodeId);
}

function scrollToBlockByIds(originBlockId, specializedBlockId, nodeId) {
    let card = document.getElementById(originBlockId);
    openCard(card);
    let section = document.getElementById(specializedBlockId);
    card.scrollIntoView({ behavior: "smooth", block: "center" });
    section.classList.add('glowing');
    setTimeout(() => section.classList.remove('glowing'), 2000);
    centerOnBlockInNetwork(nodeId);
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
    body.style.display = 'block';
}

function linkBlockRef(originBlock, code) {
    let cfg = originBlock.cfg
    let compiler = cfg.compiler
    let pattern

    if (compiler === "gambit") {
        pattern = /#(\d+)/g
    } else if (compiler === "bigloo") {
        pattern = /\(go\s+(\d+)\)/g
    } else {
        throw new Exception("unknown compiler", compiler)
    }

    return code.replace(pattern, (match, number) => {
        let specializedBlock = cfg.getSpecializedBlock(originBlock.bbs, parseInt(number))
        if (!specializedBlock) return match; // may have mismatched a non-label

        let specializedId = getHtmlIdLocation(specializedBlock)
        let originId = getHtmlIdLocation(specializedBlock.originBlock)
        let tooltip = `usage: ${specializedBlock.usage}`
        let cls = specializedBlock.usage === 0 ? "class='low-importance-button'" : ""
        let nodeId = getNetWorkUniqueId(specializedBlock);
        return `<button ${cls} data-tooltip="${tooltip}" onclick="scrollToBlockByIds('${originId}', '${specializedId}', '${nodeId}')">${match}</button>`;
    })
}

function refreshHTML(cfg) {
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
                    ${versions.map((b) => `<span class="origin-block-card-subtitle">${b.usage}</span>`).join("")}
                </span>
            </div>
            <div class="origin-block-card-body">
                <h4>source</h4>
                <code>${escapeHtml(block.source)}</code>
                ${versions.map((b) => {
            return `
                        <span id="${getHtmlIdLocation(b)}" class="origin-block-card-body-row">
                            <h4>Block #${b.id} (usage: ${b.usage})</h4>
                            <h5>Context</h5>
                            <code>${b.context}</code>
                            <h5>Code</h5>
                            <code>${b.details ? linkBlockRef(b, escapeHtml(b.details)) : linkBlockRef(b, escapeHtml(b.context))}</code>
                        </span>`
        }).join("")}
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
    refreshHTML(SBBVControlFlowGraph)
    refreshGraph(SBBVControlFlowGraph)
}

function loadCFGFromFile(file) {
    var reader = new FileReader()
    reader.onload = e => loadJSON(e.target.result)
    reader.readAsText(file)
}

if (fileInput.files[0]) loadCFGFromFile(fileInput.files[0]);

fileInput.addEventListener('change', function (event) {
    var file = event.target.files[0]
    if (file) loadCFGFromFile(file);
});
