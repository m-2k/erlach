// Shared

function trim(str) { return str.replace(/^\s+|\s+$/g, ''); };
function unrich(id) { var x = qi(id);
    x && x.addEventListener('paste', function(e) { e.preventDefault();
        document.execCommand('insertHTML',false,
            e.clipboardData.getData('text/plain').replace(/\r?\n/g,'<br/>')); });
}

// History API
function push_state(replace,state,title,url) {
    window.document.title = title;
    var s = {state: state, title: title};
    replace ? history.replaceState(s,title,url) :history.pushState(s,title,url);
};
window.addEventListener('popstate', function(e){
    var s = e.state;
    if(s) { ws.send(enc(tuple(atom('client'),tuple(atom('history'),bin(s.state)))));
        window.document.title = s.title; }
},false);

// Variables
var timeouts = [];
var intervals = [];
var pid; // page id

function init(page_id) {
    // Clear all variables and stop timers
    pid = page_id;
    timeouts.forEach(function(t){ if(t){ clearTimeout(t); if(debug) console.log('timeout cleared'); }});
    timeouts = [];
    intervals.forEach(function(t){ if(t){ clearInterval(t); if(debug) console.log('interval cleared'); }});
    intervals = [];
    
    // Run funs
    storeToLocalStorage();
};
function storeToLocalStorage() {
    intervals.push(window.setInterval(function() {
        var me = qs('#input .post-message');
        if (me) {
            var t = me.innerText, p = window.location.pathname+':message';
            t ? localStorage.setItem(p,t) : localStorage.removeItem(p);
            if(debug) console.log(t ? 'message stored to localstorage' : 'message removed from localstorage');
        }
        var te = qs('#input .post-topic');
        if (te) {
            var t = te.innerText, p = window.location.pathname+':topic';
            t ? localStorage.setItem(p,t) : localStorage.removeItem(p);
            if(debug) console.log(t ? 'topic stored to localstorage' : 'topic removed from localstorage');
        }
    },5000));
};
function removeFromLocalStorage() {
    localStorage.removeItem(window.location.pathname+':topic');
    localStorage.removeItem(window.location.pathname+':message');
};
function loadFromLocalStorage() {
    var me = qs('#input .post-message');
    if (me) {
        me.innerText = localStorage.getItem(window.location.pathname+':message');
        if(debug) console.log('message loaded from localstorage');
    }
    var te = qs('#input .post-topic');
    if (te) {
        te.innerText = localStorage.getItem(window.location.pathname+':topic');
        if(debug) console.log('topic loaded from localstorage');
    }
};

(function drag() {
    var drag = qs('.drag');
    if(drag) {
        var hideTimer; // There is no need to save

        var hideDrag = function() {
            hideTimer = window.setTimeout(function() { drag.style.display = "none"; }, 50);
        };

        document.body.addEventListener("dragover", function(e) {
            e.stopPropagation();
            e.preventDefault();
            if (hideTimer) { clearTimeout(hideTimer); }
            drag.style.display = "block";
        }, true);
        document.body.addEventListener("dragleave", hideDrag, true);
        drag.addEventListener("drop", function(e) { // For Form (overloaded)
          e.stopPropagation();
          e.preventDefault();
          hideDrag();
          handleFileSelect(e);
        }, false);
    };
})();

function setCanvasSize(imContainerId,width,height) { // prevent flashing
    var cnv=qs('#'+imContainerId+' canvas');
    if(cnv.width !== width || cnv.height !== height) {
        cnv.width=width; cnv.height=height;
    }
};

function uploadFileStarting(state) {
    var p = qs('#input .image-process');
    p.classList.add('visibled');
};
function uploadFileShowState(imContainerId,state) {
    var p = qs('#'+imContainerId+' .image-process');
};
function uploadFileFinished(imContainerId) {
    var p = qs('#'+imContainerId+' .image-process');
    if(p) {
        p.classList.remove('visibled');
        p.remove();
    }
};

function previewAndUploadFile(file) {
    var reader  = new FileReader();
    reader.onloadend = function () {
        var img = new Image();
        img.onload = function () {
            uploadFile(file, this.width, this.height);
            var cnv = qs('#input canvas');
            cnv.width = this.width;
            cnv.height = this.height;
            var ctx = cnv.getContext('2d');
            ctx.drawImage(this, 0, 0);
            qs('#input .post-image').classList.remove('empty');
        };
        img.src = reader.result;
    };
    reader.readAsDataURL(file);
};

function uploadFile(file, width, height) {
    var imContainerId = qs('#input .post-image').id;
    uploadFileShowState(imContainerId,'Uploading');
    ftp.autostart = true;
    ftp.sid = pid;
    ftp.filename = Date.now().toString();
    ftp.meta = tuple(atom('meta'),bin(imContainerId),number(width),number(height));
    ftp.init(file);
};
function handleFileSelect(evt) {
    var files = evt.dataTransfer.files;
    
    for (var i = 0, file; file = files[i]; i++) {
        if (!file.type.match('^(image/jpeg)|(image/png)|(image/gif)$')) {
            console.log("UNACCEPTABLE!: " + file.type); continue;
        }
        uploadFileStarting('Preview');
        previewAndUploadFile(file);
        break;
    }
};

var bpgw = new Worker('/static/bpgdec8-ww.js');
bpgw.onmessage = function(e) {
    switch(e.data.type) {
        case 'log': console.log('Worker log:' + e.data.message); break;
        case 'debug': console.log('Worker debug:' + e.data.data); debugger; break;
        case 'res':
            var imData = e.data.image_data;
            var cnv = qs('#'+e.data.meta+' canvas');
            cnv.width = imData.width;
            cnv.height = imData.height;
            
            var ctx = cnv.getContext('2d');
            ctx.putImageData(imData, 0, 0);

            uploadFileFinished(e.data.meta);
            break;
        default: console.log('Unknown event:' + e.data);
    };
};

ftp.relay = function(rsp) {
    var imContainerId = rsp.v[3].v;
    uploadFileShowState(imContainerId,'Decoding');
    bpgw.postMessage({type:'image', img:rsp.v[7].v, meta:imContainerId});
};
