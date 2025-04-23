// Shared
function je(e) { return JSON.stringify(e) };
function jd(e) { console.log("RAW DATA:", e); return JSON.parse(e) };
function ls(k,v) { try { return v ? localStorage.setItem(je(k),je(v)) : jd(localStorage.getItem(je(k))) } catch(e) {} };

function lsrem(k) { localStorage.removeItem(je(k)) };
function lsremall() { localStorage.clear() };
function ts() { return Date.now() / 1000 | 0; };

(function() {
  var requestAnimationFrame = window.requestAnimationFrame || window.mozRequestAnimationFrame ||
                              window.webkitRequestAnimationFrame || window.msRequestAnimationFrame;
})();


function sub() {
    // var t1 = window.performance.now();
    var lpack = function(l) { return list(l.map(function(e) { return number(e) }) ) };
    var r = [];
    for (var k in localStorage) {
        var kn = jd(k);
        if(kn instanceof Array && kn[0] === 'sub' ) {
            switch(kn[1]) {
                case 't': var v=jd(localStorage.getItem(k));
                    r.push(tuple(tuple(bin(kn[1]),number(kn[2])),
                        tuple(number(v[0]),number(v[1]),number(v[2]),lpack(v[3]),number(v[4]),number(v[5]),lpack(v[6]),number(v[7])))); break;
                case 'p': var v=jd(localStorage.getItem(k));
                    r.push(tuple(tuple(bin(kn[1]),number(kn[2])),
                        tuple(number(v[0]),number(v[1]),number(v[2]),lpack(v[3]),lpack(v[4])))); break;
            }}}
    // var t2 = window.performance.now(); console.log("time: "+(t2-t1));
    return list(r);
};

function trim(str) { return str.replace(/^\s+|\s+$/g, ''); };
function unrich(id) { var x = qi(id);
    x && x.addEventListener('paste', function(e) {
        e.preventDefault();
        var text = e.clipboardData.getData('text/plain'); //.replace(/\r?\n/g,'<br/>');
        document.execCommand('insertHTML',false, text);
    });
}
function cursorEnd(el){ // http://stackoverflow.com/questions/1125292/how-to-move-cursor-to-end-of-contenteditable-entity/3866442#3866442
    var r = document.createRange(); r.selectNodeContents(el); r.collapse(false);
    var s = window.getSelection(); s.removeAllRanges(); s.addRange(r);
};

function append(el,text) {
    el.appendChild(document.createTextNode(text));
    cursorEnd(el);
};

// window.onscroll = function() { };
function scrollToPost(id) {
    var p=qi(id);
    if(p && p.parentNode.id === 'posts-new') {
        p.parentNode.removeAttribute('id'); // trick
        var s=qs('#posts-new-controls .control-show');
        s && s.click();
    }
    scrollToElement(id);
}
function scrollToTop() { window.scrollTo(0, 0) };
function scrollToElement(id) {
    var e = qi(id);
    if(e) {
        var rect = e.getBoundingClientRect(), hh = qi('header').clientHeight;
        if(rect.top <= hh || rect.bottom >= window.innerHeight) { e.scrollIntoView(); window.scrollBy(0,-hh) };
        flashPost(id);
    }
};
function flashPost(id) {
    var el = qs('#'+id+' .post-content');
    if(el) {
        el.style.webkitAnimationName = el.style.animationName = '';
        window.setTimeout(function() {
            if(el) { el.style.webkitAnimationName = el.style.animationName = 'flash-element'; }
        }, 4);
    }
};

// History API
function push_state(replace,state,title,url) {
    window.document.title = title;
    var s = { state: state, title: title };
    replace ?history.replaceState(s,title,url) : history.pushState(s,title,url);
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
var enc_queue_1 = [];
var enc_queue_2 = [];
var enc_process_1;
var enc_process_2;

function init(page_id) {
    // Clear all variables and stop timers
    pid = page_id;
    timeouts.forEach(function(t){ if(t){ clearTimeout(t); if(debug) console.log('timeout cleared'); }});
    timeouts = [];
    intervals.forEach(function(t){ if(t){ clearInterval(t); if(debug) console.log('interval cleared'); }});
    intervals = [];
    enc_queue_1 = [];
    enc_queue_2 = [];
    enc_process_1 = false;
    enc_process_2 = false;
    
    // Run funs
    textStore();
};

function textStoreKey(key) { return ['text',window.location.pathname,key]; };
function textStore() {
    var store = function(sel,key) {
        var e = qs(sel), k = textStoreKey(key);
        (e && e.innerText != "") ? ls(k,[ts(),e.innerText]) : lsrem(k);
    };
    intervals.push(window.setInterval(function() {
        store('#input .post-topic','t');
        store('#input .post-message','m');
    },5000));
};
function textErase() { lsrem(textStoreKey('t')); lsrem(textStoreKey('m')); };
function textRestore() {
    var restore = function(sel,key) {
        var e = qs(sel), v = ls(textStoreKey(key));
        if(e && v) { e.innerText = v[1] };
    };
    restore('#input .post-topic','t');
    restore('#input .post-message','m');
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
        // document.body.addEventListener("drop", hideDrag, true);  // For Body
        drag.addEventListener("drop", function(e) { // For Form (overloaded)
          e.stopPropagation();
          e.preventDefault();
          hideDrag();
          handleFileSelect(e);
        }, false);
    };
})();

function setDimension(imContainerId,width,height) { // prevent flashing
    var cnv=qs('#'+imContainerId+' .media.image');
    if(cnv.width !== width || cnv.height !== height) {
        cnv.width=width; cnv.height=height;
    }
};

function uploadFileStarting(state) {
    var p = qs('#input .image-process');
    // p && p.innerText = state;
    p && p.classList.remove('error');
    p && p.classList.add('visibled');
    var m = qs('#input .image-manage');
    m && m.classList.add('visibled');
};
function uploadFileShowState(imContainerId,state) {
    // alert(state);
    var p = qs('#'+imContainerId+' .image-process');
    // if(p) p.innerText = state;
};
function uploadFileError(imContainerId) {
    var p = qs('#'+imContainerId+' .image-process');
    p && p.classList.add('error');
};
function uploadFileFinished(imContainerId) {
    var p = qs('#'+imContainerId+' .image-process');
    if(p) {
        p.classList.remove('visibled');
    }
};

function replaceNode(destination,target) {
    requestAnimationFrame((function() {
        destination.parentNode.replaceChild(target,destination);
    }).bind(this));
    
};

function imageEnsureTag(e,targetMime) {
    var targetTag;
    switch(targetMime) {
        case 'image/gif': targetTag = 'img'; break;
        default: targetTag = 'canvas';
    }
    
    if(e.nodeName.toLowerCase() == targetTag) { return e; }
    else {
        var t = document.createElement(targetTag);
        t.classList.add('media', 'image');
        replaceNode(e,t);
        return t;
    }
};
function previewAndUploadFile(file) {
    var container = qs('#input .post-image');
    if(container) {
        var reader  = new FileReader();
        reader.onloadend = function () {
            
            var t = qs('#input .media');
            var e = imageEnsureTag(t,file.type);
            
            if(e.tagName == 'IMG') {
                e.onload = function () {
                    uploadFile(container.id, file, this.width, this.height);
                    qs('#input .post-image').classList.remove('empty');
                };
                e.src = reader.result;
            } else {
                var img = new Image();
                img.onload = function () {
                    uploadFile(container.id, file, this.width, this.height);
                    // var e = qs('#input .media.image');
                    e.width = this.width;
                    e.height = this.height;
                    var ctx = e.getContext('2d');
                    ctx.drawImage(this, 0, 0);
                    qs('#input .post-image').classList.remove('empty');
                };
                img.src = reader.result;
            }
        };
        reader.readAsDataURL(file);
    }
};

function uploadFile(containerId, file, width, height) {
    uploadFileShowState(containerId,'Uploading');
    ftp.autostart = true;
    ftp.sid = pid;
    ftp.filename = Date.now().toString();
    ftp.meta = tuple(atom('meta'),bin(containerId),number(width),number(height));
    ftp.init(file);
};
function handleFileSelect(evt) {
    var files = evt.target.files || evt.dataTransfer.files; // input or drag
    for (var i = 0, file; file = files[i]; i++) {
        if (!file.type.match('^(image/jpeg)|(image/png)|(image/gif)$')) {
            console.log("UNACCEPTABLE!: " + file.type); continue;
        }
        uploadFileStarting('Preview');
        previewAndUploadFile(file);
        break;
    }
};

var bpgw = new Worker('/static/bpgdec8a-096-ww.js');
bpgw.onmessage = function(e) {
    switch(e.data.type) {
        case 'log': console.log('Worker log: ' + e.data.message); break;
        case 'debug': console.log('Worker debug: ' + e.data.data); debugger; break;
        case 'res':
            if(e.data.meta.page_id == pid) {
                var img = e.data.image;
                var frames = e.data.frames;
                var loop_count = e.data.loop_count;
                var cnv = qs('#' + e.data.meta.container + ' .media.image');
                cnv.width = img.width;
                cnv.height = img.height;
            
                var ctx = cnv.getContext('2d');
                // ctx.putImageData(img, 0, 0);
            
                (function() {
                    function d() {
                        var a = img.n;
                        ++a >= frames.length && (0 == loop_count || img.q < loop_count ? (a = 0, img.q++) : a =- 1);
                        0 <= a && (img.n = a, ctx.putImageData(frames[a].img, 0, 0), setTimeout(d, frames[a].duration))
                    };
                    ctx.putImageData(img, 0, 0);
                    frames.length > 1 && (img.n = 0, img.q = 0, setTimeout(d, frames[0].duration));
                }.bind(ctx,img,frames,loop_count))();

                cnv.onclick = viewerShowImage; // redefining
            
                uploadFileFinished(e.data.meta.container);
                if(enc_queue_1.length > 0) { bpgw.postMessage(enc_queue_1.shift()); }
                else { enc_process_1 = false; };
            }
            else { console.log('Skip rendering (page outdated) pid=' + e.data.meta.page_id); };
            break;
        default: console.log('Unknown event:' + e.data);
    };
};

ftp.relay = function(rsp) {
    var meta = {container:rsp.v[3].v[0].v, mime:rsp.v[3].v[1].v, page_id:pid, width:rsp.v[3].v[2].v, height:rsp.v[3].v[3].v };
    uploadFileShowState(meta.container,'Decoding');
    
    var item = {type:'image', img:rsp.v[7].v, meta:meta };

    switch(item.meta.mime) {
        case 'image/gif':
        case 'image/jpeg':
            if(enc_process_2 == false) { enc_process_2 = true; decodeNativeImage(item); }
            else { enc_queue_2.push(item); };
            break;
        default:
            if(enc_process_1 == false) { enc_process_1 = true; bpgw.postMessage(item); }
            else { enc_queue_1.push(item); }
    }
};
function decodeNativeImage(item) { // type selector
    switch(item.meta.mime) {
        case 'image/jpeg': decodeNativeImageToCanvas(item); break;
        default: decodeNativeImageToImageNode(item); break;
    }
};
function decodeNativeImageToCanvas(item) {
    
    var url = URL.createObjectURL(new Blob([item.img]));

    var im = new Image();
    im.onload = function () {
        var cnv = qs('#' + item.meta.container + ' .media.image');
        if(cnv) {
            cnv.width = im.width;
            cnv.height = im.height;

            var ctx = cnv.getContext('2d');
            ctx.drawImage(im, 0, 0);
            cnv.onclick = viewerShowImage; // redefining
            uploadFileFinished(item.meta.container);
        
            if(enc_queue_2.length > 0) { decodeNativeImage(enc_queue_2.shift()); }
            else { enc_process_2 = false; };
        };
        URL.revokeObjectURL(url);
    }
    im.src = url;
};
function decodeNativeImageToImageNode(item) {
    var url = URL.createObjectURL(new Blob([item.img]));
    var dest = qs('#' + item.meta.container + ' .media.image');
    var im = new Image();
    im.classList.add('media', 'image');
    replaceNode(dest,im)
    
    
    im.onload = function () {
        im.onclick = viewerShowImage; // redefining
        uploadFileFinished(item.meta.container);
        
        if(enc_queue_2.length > 0) { decodeNativeImage(enc_queue_2.shift()); }
        else { enc_process_2 = false; };
        // URL.revokeObjectURL(url); for reusing in viewer
    };
    im.src = url;
    
};

qi('viewer').addEventListener('click',function(evt) { viewerClose(); });

function canvasClone(source,dest) { // http://stackoverflow.com/a/7141590
    dest.width = source.width;
    dest.height = source.height;
    dest.getContext('2d').drawImage(source, 0, 0);
};
function viewerShowImage(e) {
    switch(e.target.nodeName) {
        case 'CANVAS':
            var cnv = document.createElement('canvas');
            cnv.classList.add('media', 'image');
            var cont = qs('#viewer .monitor');
            replaceNode(cont.childNodes[0],cnv);
            canvasClone(e.target,cnv);
            break;
        case 'IMG':
            var im = document.createElement('img');
            im.classList.add('media', 'image');
            var cont = qs('#viewer .monitor');
            replaceNode(cont.childNodes[0],im);
            im.src = e.target.src;
            break;
    };
    
    qi('viewer').classList.add('show');
};
function viewerClose() {
    qi('viewer').classList.remove('show');
};

/* DRUG AND DROP VISUAL EFFECTS END */
