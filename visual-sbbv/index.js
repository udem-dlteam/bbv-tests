// Display

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

// Control Flow Graph

SBBVControlFlowGraph = null;

class SpecializedCFG {
    #specializedBlocks
    #originBlocks

    constructor() {
        this.#specializedBlocks = {}
        this.#originBlocks = {}
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

    addBlock({ id, bbs, origin, context, details, source, usage, predecessors }) {
        let originBlock = this.#getOrSetOriginBlock(bbs, origin, source);
        let specializedBlock = new SpecializedBasicBlock(id, originBlock, context, details, usage, predecessors, this)

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

    constructor(id, originBlock, context, details, usage, predecessors, cfg) {
        this.id = id
        this.originBlock = originBlock
        this.context = context
        this.details = details
        this.usage = usage
        this.#predecessors = predecessors
        this.cfg = cfg
    }

    get bbs() {
        return this.originBlock.bbs
    }

    get predecessors() {
        return this.#predecessors.map((id) => this.cfg.getSpecializedBlock(this.bbs, id))
    }
}

function parseCFG(jsonBlocks) {
    let cfg = new SpecializedCFG();

    for (let block of jsonBlocks) {
        cfg.addBlock(block)
    }

    return cfg
}

// IO

function getHtmlIdLocation(block) {
    if (block instanceof SpecializedBasicBlock) {
        return getHtmlIdLocation(block.originBlock)
    } else if (block instanceof OriginBasicBlock) {
        return `${block.bbs}-${block.id}`
    }
    throw "not a block", block
}

function scrollToBlockCard(id) {
    let card = document.getElementById(id);
    openCard(card)
    card.scrollIntoView({behavior: "smooth", block: "center"});
    card.classList.add('glowing');
    setTimeout(() => card.classList.remove('glowing'), 2000);
}

function escapeHtml(unsafe) {
    return unsafe
        .replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
        .replace(/"/g, "&quot;")
        .replace(/'/g, "&#039;");
}

function toggleCard(cardElement) {
    const body = cardElement.querySelector('.origin-block-card-body');
    body.style.display = body.style.display === 'block' ? 'none' : 'block';
}

function openCard(cardElement) {
    const body = cardElement.querySelector('.origin-block-card-body');
    body.style.display = 'block';
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
                    function linkBlockRef(code) {
                        return code.replace(/#(\d+)/g, (match, number) => {
                            let loc = getHtmlIdLocation(cfg.getSpecializedBlock(b.bbs, parseInt(number)))
                            return `<button onclick="scrollToBlockCard('${loc}')">#${number}</button>`;
                        })
                    }

                    return `
                        <span class="origin-block-card-body-row">
                            <h4>Block #${b.id} (usage: ${b.usage})</h4>
                            <code>${b.details ? linkBlockRef(escapeHtml(b.details)) : linkBlockRef(escapeHtml(b.context))}</code>
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
        var { specializedCFG } = JSON.parse(content)
    } catch (error) {
        alert(error)
        return;
    }
    SBBVControlFlowGraph = parseCFG(specializedCFG)
    refreshHTML(SBBVControlFlowGraph)
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
