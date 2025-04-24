
// Variables
var timeouts = [];
var intervals = [];
var pid; // page id
var enc_queue_1 = [];
var enc_queue_2 = [];
var enc_process_1;
var enc_process_2;

var URI_GOOGLE_IMG_SEARCH='https://www.google.com/searchbyimage?&image_url=';
var URI_TINEYE_IMG_SEARCH='https://www.tineye.com/search?pluginver=bookmark_1.0&url=';

var lazyLoader;
var bpgw = new Worker('/static/bpgdec.min.js');

(function() {
    window.requestAnimationFrame = window.requestAnimationFrame || window.mozRequestAnimationFrame ||
        window.webkitRequestAnimationFrame || window.msRequestAnimationFrame;
})();

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
    applySettings();
};

// 
// Local Storage
// 
function nodeName(e) { return (e && e.nodeName) ? e.nodeName.toLowerCase() : 'unknown'; }
function je(e) { return JSON.stringify(e) };
function jd(e) { return JSON.parse(e) };
function ls(k,v) { try { return v ? localStorage.setItem(je(k),je(v)) : jd(localStorage.getItem(je(k))) } catch(e) {} };

function lsrem(k) { localStorage.removeItem(je(k)) };
function lsremall() { localStorage.clear() };
function ts() { return Date.now() / 1000 | 0; };

function sub() {
    // var t1 = window.performance.now();
    var lpack = function(l) { return list(l.map(function(e) { return number(e) }) ) };
    var r = [];
    for (var i = 0; i < localStorage.length; i++) { 
        var k = localStorage.key(i);
        try {
            var kn = jd(k);
        
            if(kn instanceof Array && kn[0] === 'sub' ) {
                switch(kn[1]) {
                    case 't': var v=jd(localStorage.getItem(k));
                        r.push(tuple(tuple(bin(kn[1]),number(kn[2])),
                            tuple(number(v[0]),number(v[1]),number(v[2]),lpack(v[3]),number(v[4]),number(v[5]),lpack(v[6]),number(v[7])))); break;
                    case 'p': var v=jd(localStorage.getItem(k));
                        r.push(tuple(tuple(bin(kn[1]),number(kn[2])),
                            tuple(number(v[0]),number(v[1]),number(v[2]),lpack(v[3]),lpack(v[4])))); break;
                }}
        } catch (err) { console.log('localStorage broken item: ' + k + '. Removed.'); localStorage.removeItem(k); continue; }
    }
    // var t2 = window.performance.now(); console.log("time: "+(t2-t1));
    return list(r);
};


// 
// Text Boxes
// 
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

window.onscroll = function() {
    if(lazyLoader && (lazyLoader.getBoundingClientRect().top < window.document.body.clientHeight)) {
        lazyLoader.dispatchEvent(new CustomEvent("lazy", { bubbles: false, cancelable: false, detail: false }));
        lazyLoader = undefined;
        console.log('lazy');
    }
};

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
function scrollToBottom() { window.scrollTo(0,document.body.scrollHeight) };
function scrollToElement(id,disableHighlight) {
    var e = qi(id);
    if(e) {
        var hdr = qi('header');
        var rect = e.getBoundingClientRect(), hh = (hdr && hdr.clientHeight) || 0;
        if(rect.top <= hh || rect.bottom >= window.innerHeight) { e.scrollIntoView(); window.scrollBy(0,-hh) };
        disableHighlight || flashPost(id);
    }
};
function flashPost(id) {
    var el = qs('[id="'+id+'"] .post-content');
    if(el) {
        el.style.webkitAnimationName = el.style.animationName = '';
        window.setTimeout(function() {
            if(el) { el.style.webkitAnimationName = el.style.animationName = 'flash-element'; }
        }, 4);
    }
};

// History API
function push_state(replace,state,title,url,board,thread) {
    window.document.title = title;
    var s = {
        state: state,
        title: title,
        board: board,
        thread: thread
    };
    replace ? history.replaceState(s,title,url) : history.pushState(s,title,url);
};
window.addEventListener('popstate', function(e){
    var s = e.state;
    if(s) { ws.send(enc(tuple(atom('client'),tuple(atom('history'),bin(s.state)))));
        window.document.title = s.title; }
},false);

// function textStoreKey(key) { return ['text',window.location.pathname,key]; };
function textStoreKey(key) { return ['text',[history.state.board,history.state.thread],key]; };
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
        drag.addEventListener("drop", function(e) { // For Form (overloaded)
          e.stopPropagation();
          e.preventDefault();
          hideDrag();
          handleFileSelect(e);
        }, false);
    };
})();

function uploadFileStarting(state) {
    var p = qs('#input .image-process');
    p && p.classList.remove('error');
    p && p.classList.add('visibled');
    var m = qs('#input .image-manage');
    m && m.classList.add('visibled');
};
function uploadFileShowState(imContainerId,state) {
    var p = qs('[id="'+imContainerId+'"] .image-process');
};
function uploadFileError(imContainerId) {
    var p = qs('[id="'+imContainerId+'"] .image-process');
    p && p.classList.add('error');
};
function uploadFileFinished(imContainerId) {
    var p = qs('[id="'+imContainerId+'"] .image-process');
    p && p.classList.remove('visibled');
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
    var files = evt.target.files || evt.dataTransfer.files;
    var type = files[0].type === "" && files[0].name.endsWith('.bpg') ? 'image/bpg' : files[0].type;
    
    switch(type) {
        case 'image/jpeg':
        case 'image/png':
        case 'image/gif':
            uploadFileStarting('Preview');
            previewAndUploadFile(files[0],type);
            break;
        case 'image/bpg':
            uploadFileStarting('Preview');
            previewAndUploadBpgFile(files[0]);
            break;
        default:
            console.log("Unacceptable file type");
    }
    
};

function previewAndUploadFile(file) {
    var container = qs('#input .post-image');
    var img = container.querySelector('img.media.image');
    var cnv = container.querySelector('canvas.media.image');
    var temp = new Image(); // for preventing infinity iteration of img.media
    
    img.setAttribute('onload','shadowOnLoad(this);');
    img.setAttribute('onerror','shadowOnError(this);');
    
    if(container) {
        var reader  = new FileReader();
        reader.onloadend = function () {
            temp.onload = function () {
                uploadFile(container.id, file, this.width, this.height);
                if(file.type == 'image/gif') {
                    img.src = this.src;
                    img.classList.remove('hidden');
                    cnv.classList.add('hidden');
                    setActionOnImage(img);
                } else {
                    cnv.width = this.width;
                    cnv.height = this.height;
                    cnv.getContext('2d').drawImage(this, 0, 0);
                    img.classList.add('hidden');
                    cnv.classList.remove('hidden');
                    setActionOnImage(cnv);
                }
            
                container.classList.remove('empty');
            };
            temp.src = reader.result;
        };
        reader.readAsDataURL(file);
    }
};

function previewAndUploadBpgFile(file) {
    var container = qs('#input .post-image');
    uploadFile(container.id, file, 10, 10);
    
    if(container) {
        var reader  = new FileReader();
        reader.onloadend = function () {
            
            var meta = {container:'input', mime:'image/bpg', page_id:pid };
            var item = {type:'image', img:reader.result, meta:meta };
            bpgw.postMessage(item);
            qs('#input .post-image').classList.remove('empty');
        };
        reader.readAsArrayBuffer(file);
    }
};

function bindExternalUri(uri) {
    qs('#input .post-image').setAttribute('data-open-external',uri);
    var im = qs('#input .media.image:not(.hidden)');
    im.onclick = undefined;
    setActionOnImage(im);
};

function parentSelector(e,selector) {
    var parent = e.parentNode;
    while (parent.parentNode.querySelector(selector) !== parent) { parent = parent.parentNode; }
    return parent;
};

function getLink(e) { return parentSelector(e,".media-link") };
function getImage(e) { return getLink(e).querySelector('.media.image:not(.hidden)') };

function setActionOnImage(e) {
    var srv = !!qs('#main.services');
    var cnt = e.parentNode.parentNode;
    if(cnt.dataset.openExternal) {
        e.parentNode.setAttribute('href',cnt.dataset.openExternal);
        e.parentNode.setAttribute('target','_blank');
    }
    else if(srv){
        // skip
    } else {
        getLink(e).onclick = viewerShowImage;
    }
};

bpgw.onmessage = function(e) {
    switch(e.data.type) {
        case 'log': console.log('Worker log: ' + e.data.message); break;
        case 'debug': console.log('Worker debug: ' + e.data.data); debugger; break;
        case 'res':
            if(e.data.meta.page_id == pid) {
                var img = e.data.image;
                var frames = e.data.frames;
                var loop_count = e.data.loop_count;
                var cnv = qs('[id="' + e.data.meta.container + '"] canvas.media.image');
                cnv.width = img.width;
                cnv.height = img.height;
            
                var ctx = cnv.getContext('2d');
            
                (function() {
                    function d() {
                        var a = img.n;
                        ++a >= frames.length && (0 == loop_count || img.q < loop_count ? (a = 0, img.q++) : a =- 1);
                        0 <= a && (img.n = a, ctx.putImageData(frames[a].img, 0, 0), setTimeout(d, frames[a].duration))
                    };
                    ctx.putImageData(img, 0, 0);
                    frames.length > 1 && (img.n = 0, img.q = 0, setTimeout(d, frames[0].duration));
                }.bind(ctx,img,frames,loop_count))();

                cnv.classList.remove('hidden');
                setActionOnImage(cnv);
                
                uploadFileFinished(e.data.meta.container);
            }
            else { console.log('Skip rendering (page outdated) pid=' + e.data.meta.page_id); };
            break;
        default: console.log('Unknown event:' + e.data);
    };
};

function replaceNode(destination,target) {
    requestAnimationFrame((function() {
        destination.parentNode.replaceChild(target,destination);
    }).bind(this));
    
};


function canvasClone2(source) {
    var cnv = source.cloneNode(false);
    cnv.getContext('2d').drawImage(source, 0, 0);
    return cnv;
};


function animate(fun_or_e, animation) {
    var setup=function(e,a) { e.style.webkitAnimationName = e.style.animationName = a; };
    var take=function(e) { return typeof e === 'function' ? e() : e };
    
    setup(take(fun_or_e), '');
    window.setTimeout(function() { setup(take(fun_or_e), animation); }, 4);
}

/*********************** VIEWER BEGIN ***********************/

var viewer;
(function viewerInit() {
    viewer = qi('viewer'), image = qs('#viewer .media');
    if(viewer && image) {
        viewer.active = false;
        viewer.images = [];
        viewer.position = -1;
        viewer.image = image;
        viewer.addEventListener('click',function(e) { viewerClose(e); });
    }
})();
function viewerCollectImages() {
    return Array.prototype.slice.call(document.querySelectorAll('#content .media.image:not(.hidden)'));
};
function viewerOpen(e) {
    requestAnimationFrame(function() { // second step (thread #1, RAF)
        vieverAnimate('viewer-open', function() { return viewer; });
        viewer.classList.add('active');
        viewer.images = viewerCollectImages();
        viewer.position = viewer.images.indexOf(e);
        viewer.active = true;
        viewer.focus();
    });
};
function viewerShowImage(e) {
    e = e.target || e;    
    e = getImage(e); // when processing enabled
    
    var n, url, p = e.previousSibling;
    switch(nodeName(e)) {
        case 'canvas':
            if(nodeName(p) === 'img' && p.src && getType(p) !== 'bpg') {
                n = e.cloneNode(false);
                n.width = p.width;
                n.height = p.height;
                n.getContext('2d').drawImage(p, 0, 0);
                url = p.src;
            } else {
                n = canvasClone2(e);
                url = e.src;
            }
            break;
        default:
            n = e.cloneNode(false);
            url = e.src;
    }
    
    replaceNode(viewer.image, n);     // first step (thread #0)
    viewer.image = n;
    qi('viewer-download').href=url;
    qi('viewer-google').href=URI_GOOGLE_IMG_SEARCH+encodeURIComponent(url);
    qi('viewer-tineye').href=URI_TINEYE_IMG_SEARCH+encodeURIComponent(url);
    
    if(!viewer.active) {
        viewerOpen(e);
    }
};
function vieverAnimate(animation, fun_e) {
    var fun_e = fun_e || function() { return viewer.image; };
    animate(fun_e, animation);
}
function viewerNext() {
    if(viewer && viewer.active && viewer.images.length > 1) {
        var newPos = viewer.position;
        (++newPos >= viewer.images.length) && ( newPos = 0 );
        var next = viewer.images[newPos];
        !(next.src && next.src.endsWith('.gif')) && vieverAnimate('right-sliding');
        viewer.position = newPos;
        viewerShowImage(viewer.images[viewer.position]);
        return true;
    }
};
function viewerPrev() {
    if(viewer && viewer.active && viewer.images.length > 1) {
        var newPos = viewer.position;
        (--newPos < 0) && ( newPos = viewer.images.length - 1 );
        var prev = viewer.images[newPos];
        !(prev.src && prev.src.endsWith('.gif')) && vieverAnimate('left-sliding');
        viewer.position = newPos;
        viewerShowImage(viewer.images[viewer.position]);
        return true;
    }
};
function viewerClose(e) {
    if(viewer && viewer.active && (e ? nodeName(e.target) !== 'a' : true)) {
        viewer.classList.remove('active');
        viewer.images = [];
        viewer.active = false;
        return true;
    }
};

/*********************** VIEWER END ***********************/



function imgLoad(key,url) {
    var im = qs('[id="'+key+'"] img.media.image');
    if(im) { im.src = url; } else { console.log('Unable img loading: ' + key + ', ' + url); }
};

function getType(e) {
    if(e.src.startsWith('data:image/gif;base64') || e.src.endsWith('.gif')) return 'gif';
    switch(e.src.substr(-4,4)) {
        case '.jpg': return 'jpeg';
        case '.png': return 'png';
        case '.bpg': return 'bpg';
    }
}
function shadowOnLoad(shadowed) {
    
    shadowed = shadowed.target || shadowed;
    shadowed.removeAttribute('onload');
    shadowed.removeAttribute('onerror');
    var container = shadowed.parentNode.parentNode.id;
    uploadFileShowState(container,'Decoding');
    var canvas = shadowed.nextSibling;

    switch(getType(shadowed)) {
        case 'bpg': console.log('Logic error 1'); break;
        case 'gif':
            // requestAnimationFrame((function() {
                shadowed.classList.remove('hidden');
                canvas.classList.add('hidden');
            // }).bind(this));
            setActionOnImage(shadowed);
            uploadFileFinished(container);
            break;
        default:
            shadowed.classList.add('hidden');
            drawInterpolatedThumbCanvas(canvas,shadowed);
            setActionOnImage(canvas);
            uploadFileFinished(container);
    }
};
function shadowOnError(shadowed) { };
function shadowOnLoadBpg(container) {
    uploadFileShowState(container,'Decoding BPG');
    var url = qs('[id="'+container+'"] div.media.image').dataset.url;
    var req = new XMLHttpRequest();

    req.open("GET", url, true);
    req.responseType = "arraybuffer";
    req.onload = function(e) {
        var meta = {container:container, mime:'image/bpg', page_id:pid };
        var item = {type:'image', img:req.response, meta:meta };
        bpgw.postMessage(item);
    }
    req.send();
};

function drawInterpolatedThumbCanvas(c,i) {
    var w, h, r = window.devicePixelRatio || 1;
    
    if(c.parentNode.parentNode.dataset.stream) {
        h = (c.clientHeight * r) > i.height ? i.height : (c.clientHeight * r);
        w = h * (i.width / i.height);
    } else {
        var cw = c.clientWidth || parseInt(getComputedStyle(c.parentNode.parentNode).maxWidth); // for hidden elements
        w = (cw * r) > i.width ? i.width : (cw * r);
        h = w * (i.height / i.width);
    }
    
    // более четкое на превьюхах (лучше смотрится), медленнее 13162.4
    // c.getContext('2d').drawImage(i,  0, 0, i.width, i.height , 0, 0, c.width, c.height);
    // resample_hermite(c, c.width, c.height, w, h);
    
    // фотокачество, быстрее 8123.4
    // resizeCanvasImage(i, c, w, h);

    // размытое, еще быстрее 3793.6 (баги)
    // stepBystepResampling(c,i,h);
    
    // оптимизация 2x 3145.5
    fastCanvasResampler(c, i, w, h);

};

document.addEventListener("keydown", keyHandler, false);

function keyHandler(e) {
    if(!e.metaKey && !e.ctrlKey) {
        switch (e.which) {
            case 32: viewerNext() && e.preventDefault(); break; // whitespace
            case 39: viewerNext() && e.preventDefault(); break; // arrow right
            case 37: viewerPrev() && e.preventDefault(); break; // arrow left
            case 27: viewerClose() && e.preventDefault(); break;
        }
    }
    // console.log('Key: which='+e.which+' char='+e.charCode+ ' key='+e.keyCode+' meta='+e.metaKey+' ctrl='+e.ctrlKey);
};

function fastCanvasResampler(c, i, maxw, maxh, fill) {
    var iw = i.width, ih = i.height;
    var ratio = (maxw / iw) < (maxh / ih) ? (maxw / iw) : (maxh / ih);
    
    var rounds = 2;
    var rr = ratio * rounds;
    var iwrr = iw * rr, ihrr = ih * rr;
    c.width = iwrr / rounds;
    c.height = ihrr / rounds;
  
    var c2 = c.cloneNode(false);
    c2.width = iwrr;
    c2.height = ihrr;    
    
    var ct = c.getContext("2d");
    var ct2 = c2.getContext("2d"); 
    
    
    var c3, ct3;
    if(fill) {
        ct2.fillStyle = fill;
        ct2.fillRect(0, 0, c2.width, c2.height);
    } else {
        c3 = c.cloneNode(false);
        c3.width = iwrr / 2;
        c3.height = ihrr / 2;
        ct3 = c3.getContext("2d");
    }

    var w = iw, h = ih, nw, nh, z;
    for (var s = 1; s <= rounds; s++) {
        nw = Math.trunc(iwrr / s);
        nh = Math.trunc(ihrr / s);
        
        z = (s===1) ? [ct2, i] : fill ? [ct2, c2] : [(s % 2 ? ct2 : ct3),(s % 2 ? c3 : c2)];
        
        z[0].drawImage(z[1], 0, 0, w, h, 0, 0, nw, nh);
        
        w = nw;
        h = nh;
    }
    ct.drawImage(z[0].canvas, 0, 0, w, h, 0, 0, c.width, c.height);
    delete c2;
    c3 && (delete c3)
}

// 
// Settings
// 
function loadSettings() {
    var load=function(opt) { var x=qi('option-'+opt); x && ( x.value=ls(['opt',opt]) || x.value ) };
    load('nickname');
    load('theme');
};
function saveSettings() {
    var save=function(opt) { var x=qi('option-'+opt); x && ( ls(['opt',opt],x.value) ) };
    save('nickname');
    save('theme');
    applySettings();
};
function applySettings() {
    var t = ls(['opt','theme']);
    t && ( document.body.className = t );
}
