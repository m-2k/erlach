
// Variables
var timeouts = [];
var scroll_timeout;
var intervals = [];
var pid; // page id
var page_loaded = false;
var enc_queue_1 = [];
var enc_queue_2 = [];
var enc_process_1;
var enc_process_2;
var time_warp = 0;
var language;

var URI_GOOGLE_IMG_SEARCH='https://www.google.com/searchbyimage?&image_url=';
var URI_TINEYE_IMG_SEARCH='https://www.tineye.com/search?pluginver=bookmark_1.0&url=';
var URI_SAUCENAO_IMG_SEARCH='https://saucenao.com/search.php?url=';
var URI_IQDB_IMG_SEARCH='https://iqdb.org/?url=';
var URI_WHATANIME_IMG_SEARCH='https://whatanime.ga/?url=';

var lazyLoader;
var bpgw;

function getBPGWorker() {
    if(!bpgw) {
        bpgw = new Worker('/static/bpgdec.js');
        bpgw.onmessage = BPGOnMessage;
    };
    return bpgw;
}

(function() {
    window.requestAnimationFrame = window.requestAnimationFrame || window.mozRequestAnimationFrame ||
    window.webkitRequestAnimationFrame || window.msRequestAnimationFrame || function(callback) {
        window.setTimeout(callback, 1000 / 60); };
})();

(function() {
    setTimeout(function updateTime() {
        var list = document.querySelectorAll('.post-manage .time');
        for(var i = 0; i < list.length; i++) render_time(list[i]);
        setTimeout(updateTime, 5000);
    }, 10000);
})();

function totry(e,fun) { e && fun(e) };

//
// PAGE LOAD PROCESS (NOT SESSION)
//
function init(page_id) {
    debug && console.log('init');
    // Clear all variables and stop timers
    page_loaded = false;
    pid = page_id;
    timeouts.forEach(function(t){ if(t){ clearTimeout(t); if(debug) console.log('timeout cleared'); }});
    timeouts = [];
    intervals.forEach(function(t){ if(t){ clearInterval(t); if(debug) console.log('interval cleared'); }});
    intervals = [];
    enc_queue_1 = [];
    enc_queue_2 = [];
    enc_process_1 = false;
    enc_process_2 = false;
    upload_queue = [];
    
    language = ls(['opt','language']) || 'ru';
    
    var v = qi('viewer'); v && (v.className = '');
    
    // Run funs
    textStore();
};
function finalize() {
    debug && console.log('finalize');
    page_loaded = true;
    var h = qi('header'); h && h.classList.remove('loading');
}
function terminate() {
    debug && console.log('terminate');
    page_loaded = false;
    pauseAllBpgAnimation();
    var h = qi('header'); h && h.classList.add('loading');
}


// 
// Local Storage
//
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


function message(text,className,time) {
    var pm = qi('popup-messages');
    if(pm) {
        var b = qn('button');
        className = className || 'info';
        b.classList.add(className);
        b.innerHTML = text;
        b.onclick = function(e){ b.remove(); };
        window.setTimeout(function(){ b && b.click(); },time || text.length*100+4000);
        pm.insertAdjacentElement('afterbegin', b);
    }
};

function highlight(className,time) {
    document.body.classList.add(className || 'info');
    window.setTimeout(function(){  document.body.classList.remove(className || 'info') }, time || 700);
}

function nodeName(e) { return (e && e.nodeName) ? e.nodeName.toLowerCase() : 'unknown'; }
function collapse(id) { ls(['hdn',id],true); render(id); };
function expand(id) { lsrem(['hdn',id]); render(id); };
function render(id) {
    var e = qi(id);
    if(e) {
        ls(['hdn',id]) ? e.classList.add('collapsed') : e.classList.remove('collapsed');
        
        var x = e.querySelector('.reply-action');
        x && ( x.onclick = (function() {
            var html = qn('div'), editable = qs('#input .post-message');
            
            html.innerHTML = '>>' + this + ' ';
            if(window.getSelection) {
                var s = window.getSelection().toString();
                if(s !== '') { html.innerText = '>>' + this + ' >' + s + '\n\n'; }
            }
            editable.appendChild(html);
            scrollToElement('input',true);
            editable.focus();
            cursorEnd(qs('#input .post-message'));
        }).bind(e.dataset.id) );
        x = e.querySelector('.collapse-action');
        x && ( x.onclick = (function() { collapse(this) }).bind(id) );
        x = e.querySelector('.expand-action');
        x && ( x.onclick = (function() { expand(this) }).bind(id) );
        x = e.querySelector('.pid');
        x && ( x.innerHTML = e.dataset.id );
        
        var linkList = e.querySelectorAll('a.link');
        for(var i=0; i < linkList.length; i++) {
            var a = linkList[i];
            var l = a.getAttribute('data-link');
            if(l) {
                a.onclick = (function() { scrollToPost(this); }).bind(l) ;
                a.onmouseenter = (function(dataLink, elem) {
                    flashPost(dataLink);
                    var le = qs('[id="'+dataLink+'"]'); // linked post element
                    if(le) {
                        var isAnswer = !!this.parentNode.classList.contains('post-answers');
                        requestAnimationFrame((function() {
                            elem.parentNode.insertBefore(le, isAnswer ? elem.nextSibling : elem)
                        }).bind(this));
                    }
                }).bind(a,l,e);
            }
        }
        render_time(e.querySelector('.time'));
        render_embed(e);
    }
};
function render_time(e) {
    if(e && e.dataset.ts) {
        e.innerHTML = format_timestamp(parseInt(e.dataset.ts));
    }
};
function render_image(id) {
    var e = qi(id);
    if(e) {
        var cnv = e.querySelector('canvas.media.image');
        cnv.addEventListener('bpg_loaded', function() { e.querySelector('.image-manage').classList.add('visibled') }, false);
        e.querySelector('.control.play').onclick = function() { cnv.play() };
        e.querySelector('.control.pause').onclick = function() { cnv.pause(true) };
        e.querySelector('.control.stop').onclick = function() { cnv.stop() };
        
    }
};
function render_embed(e) {
    if(ls(['opt','no-soundcloud'])) return;
    
    var sc = e.querySelectorAll('a[href^="https://soundcloud.com/"]');
    for(var i = 0; i < sc.length; i++) {
        
        if(!sc[i].href.match(/https:\/\/soundcloud\.com\/[^\/]+\/[^\/]+(?:\/|$)/i)) continue;
        
        (function (link){
            var url = 'https://soundcloud.com/oembed';
            var req = new XMLHttpRequest();
            var multi = !!(link.href.match(/https:\/\/soundcloud\.com\/[^\/]+\/(?:sets|tracks)(?:\/|$)/i));
            var body = 'format=json&iframe=true' + (multi ? '' : '&maxheight=81') + '&url=' + encodeURIComponent(link.href);

            req.open('POST', url, true);
            req.responseType = 'json';
            req.setRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
            req.onreadystatechange = function() {
                if (req.readyState === 4) {
                    if (req.status != 200) { console.error('Embed status: ' + req.status); }
                    else if(link) {
                        var r = req.response;
                        var w = document.createElement('div');
                        w.classList.add('soundcloud-embed');
                        w.innerHTML = multi ? r.html.replace('visual=true&','') : r.html;
                        replaceNode(link,w);
                    }
                }
            }
            req.send(body);
        })(sc[i]);
    }
};

function format_timestamp(ts) {
    var diff = (Date.now() - new Date(ts) + time_warp)/1000;
    var t = Math.trunc(diff/60/60/24/365); if(t > 0) return t + '<span class="ru"> г</span><span class="en"> Y</span><span class="ua"> р</span>';
        t = Math.trunc(diff/60/60/24/30);  if(t > 0) return t + '<span class="ru"> мес</span><span class="en"> Mh</span><span class="ua"> мiс</span>';
        t = Math.trunc(diff/60/60/24);     if(t > 0) return t + '<span class="ru"> дн</span><span class="en"> D</span><span class="ua"> дн</span>';
        t = Math.trunc(diff/60/60);        if(t > 0) return t + '<span class="ru"> ч</span><span class="en"> h</span><span class="ua"> г</span>';
        t = Math.trunc(diff/60);           if(t > 0) return t + '<span class="ru"> мин</span><span class="en"> min</span><span class="ua"> хв</span>';
    return  Math.trunc(diff) + '<span class="ru"> сек</span><span class="en"> sec</span><span class="ua"> сек</span>';
};


// 
// Text Boxes
// 
function trim(str) { return str; };
function unrich(id) {
    var x = qi(id);
    console.log(x);
    x && x.addEventListener('paste', function(e) {
        e.preventDefault();
        var text = e.clipboardData.getData('text/plain');
        document.execCommand('insertText',false, text);
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
        debug && console.log('lazy loading');
    }
    
    scroll_timeout && clearTimeout(scroll_timeout);
    page_loaded && ( scroll_timeout = setTimeout(storeScroll, 500) );
};

function scrollWait(id) {
    debug && console.log('scroll wait to '+id);
    
    if(id) {
        setTimeout(function tryScroll() {
            var e = qi(id);
            if(e) { scrollToPost(id); }
            else if(page_loaded) { debug && console.log('try scroll aborted'); }
            else { debug && console.log('try scroll iteration'); setTimeout(tryScroll,60); }
        },4);
    } else {
        setTimeout(function tryScrollPx() {
            var val = (history.state && history.state.scroll) || 0;
            if(document.body.scrollHeight > val) {
                debug && console.log('try scrollPx SCROLL');
                window.scrollTo(0,val)
            } else { debug && console.log('try scrollPx iteration '+document.body.scrollHeight+' '+val); setTimeout(tryScrollPx,60); }
        },4);
    }
};
function scrollToPost(id) {
    debug && console.log('scrollToPost("'+id+'");');
    var p=qi(id);
    if(p && p.parentNode.id === 'posts-new') {
        p.parentNode.removeAttribute('id'); // trick
        var s=qs('#posts-new-controls .control-show');
        s && s.click();
    }
    window.setTimeout(function() { scrollToElement(id); }, 4);
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
    var el = qs('[id="'+id+'"] .post-flash');
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
        thread: thread,
        scroll: 0
    };
    replace ? history.replaceState(s,title,url) : history.pushState(s,title,url);
};
function storeScroll() {
    if(history.state) {
        history.state.scroll = window.pageYOffset
        history.replaceState(history.state,document.title,window.location.href);
    };
}
window.addEventListener('popstate', function(e){
    var s = e.state;
    if(s) { ws.send(enc(tuple(atom('client'),tuple(atom('history'),bin(s.state)))));
        window.document.title = s.title; }
},false);

function textStoreKey(key) {
    if(history.state && history.state.board) {
        return ['text',[history.state.board,history.state.thread],key];
    }
};
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
            hideTimer = window.setTimeout(function() { drag.style.display = "none"; snowStart(); }, 10);
        };

        document.body.addEventListener("dragover", function(e) {
            snowPause();
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
    debug && console.log('uploadFileStarting');
    var p = qs('#input .image-process');
    p && p.classList.remove('error');
    p && p.classList.add('visibled');
    var m = qs('#input .image-manage');
    m && m.classList.add('visibled');
};
function uploadFileShowState(imContainerId,state) {
    debug && console.log('uploadFileShowState');
    var p = qs('[id="'+imContainerId+'"] .image-process');
};
function uploadFileError(imContainerId) {
    debug && console.log('uploadFileError');
    var p = qs('[id="'+imContainerId+'"] .image-process');
    p && p.classList.add('error');
};
function uploadFileFinished(imContainerId) {
    debug && console.log('uploadFileFinished');
    var p = qs('[id="'+imContainerId+'"] .image-process');
    p && p.classList.remove('visibled');
};

function uploadFile(containerId, file, width, height) {
    debug && console.log('uploadFile');
    uploadFileShowState(containerId,'Uploading');
    ftp.autostart = true;
    ftp.sid = pid;
    ftp.filename = performance.now().toString();
    ftp.meta = tuple(atom('meta'),bin(containerId),number(width),number(height));
    ftp.init(file);
};

function handleFileSelect(evt) {
    debug && console.log('handleFileSelect');
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
    debug && console.log('previewAndUploadFile');
    var container = qs('#input .post-image');
    var img = container.querySelector('img.media.image');
    var cnv = container.querySelector('canvas.media.image');
    var temp = new Image(); // for preventing infinity iteration of img.media
    
    if(container) {
        var reader  = new FileReader();
        reader.onloadend = function () {
            temp.onload = function () {
                setTimeout((function(cid) { uploadFile(cid, file, this.width, this.height) }).bind(this,container.id), 4);
                if(file.type == 'image/gif') {
                    img.onload = function() {
                        this.onload = undefined;
                        this.setAttribute('onload','shadowOnLoad(this);');
                        this.setAttribute('onerror','shadowOnError(this);');
                    }
                    img.src = this.src;
                    img.classList.remove('hidden');
                    cnv.classList.add('hidden');
                    setActionOnImage(img);
                } else {
                    cnv.width = this.width;
                    cnv.height = this.height;
                    cnv.getContext('2d').drawImage(this, 0, 0);
                    img.setAttribute('onload','shadowOnLoad(this);');
                    img.setAttribute('onerror','shadowOnError(this);');
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
    debug && console.log('previewAndUploadBpgFile');
    var container = qs('#input .post-image');
    uploadFile(container.id, file, 10, 10);
    
    if(container) {
        var reader  = new FileReader();
        reader.onloadend = function () {
            
            var meta = {container:'input', mime:'image/bpg', page_id:pid };
            var item = {type:'image', img:reader.result, meta:meta };
            getBPGWorker().postMessage(item);
            qs('#input .post-image').classList.remove('empty');
        };
        reader.readAsArrayBuffer(file);
    }
};

function bindExternalUri(uri) {
    debug && console.log('bindExternalUri');
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
    debug && console.log('setActionOnImage');
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

var BPGOnMessage = function(e) {
    debug && console.log('bpgw.onmessage');
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

                cnv.bpg = {image: img, frames: frames, loop_count: loop_count, animation: frames.length > 1 };
                if(cnv.bpg.animation) { setBPGAnimation(cnv);  cnv.stop(); }
                else { cnv.getContext('2d').putImageData(img, 0, 0); }
                
                cnv.dispatchEvent(new CustomEvent("bpg_loaded", { bubbles: false, cancelable: false, detail: false }));
                
                cnv.classList.remove('hidden');
                setActionOnImage(cnv);
                
                uploadFileFinished(e.data.meta.container);
            }
            else { console.log('Skip rendering (page outdated) pid=' + e.data.meta.page_id); };
            break;
        default: console.log('Unknown event:' + e.data);
    };
};

function pauseAllBpgAnimation(current) {
    var nodeList = document.querySelectorAll('canvas');
    for(var i = 0; i < nodeList.length; i++) {
        nodeList[i].pause && nodeList[i].pause();
    }
};

function setBPGAnimation(cnv) {
    var ctx = cnv.getContext('2d'), img = cnv.bpg.image,frames = cnv.bpg.frames, loop_count = cnv.bpg.loop_count;
    
    cnv.play = function() {
        if(img.pla && !img.pau) return;
        pauseAllBpgAnimation(ctx.canvas);
        img.pla = true;
        img.pau = false;
        function d() {
            if(img.pau) return;
            var a = img.n;
            ++a >= frames.length && (0 == loop_count || img.q < loop_count ? (a = 0, img.q++) : a =- 1);
            if(0 <= a) {
                img.n = a;
                requestAnimationFrame((function() {
                    ctx.putImageData(frames[a].img, 0, 0);
                    setTimeout(d, frames[a].duration);
                }).bind(ctx,frames,img,d));
            };
        };
        frames.length > 1 && setTimeout(d, frames[0].duration);
    }

    cnv.pause = (function(img,toggle) {
        if(img.pla) { img.pau ? toggle && ctx.canvas.play() : ( img.pau = true ) }
    }).bind(ctx,img);

    cnv.stop = (function() {
        frames.length > 1 && ( img.pla = false, img.pau = true, img.n = 1, img.q = 0 )
        ctx.putImageData(frames[0].img, 0, 0);
    }).bind(ctx,frames,img);
    
    cnv.dataset.animation = true;
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
    return Array.prototype.slice.call(document.querySelectorAll(
        '#content .post:not(.collapsed) .media.image:not(.hidden)' +
        ', #stream .stream-image:not(.collapsed) .media.image:not(.hidden)'
    ));
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
    
    var bpgAnimManage = qs('#viewer .image-manage');
    bpgAnimManage.classList.remove('visibled');
    
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
                url = p.dataset.url;
                n.bpg = e.bpg;
                
                if(n.bpg && n.bpg.animation) {
                    setBPGAnimation(n);
                    n.pause();
                    
                    qs('#viewer .control.play').onclick = function() { n.play() };
                    qs('#viewer .control.pause').onclick = function() { n.pause(true) };
                    qs('#viewer .control.stop').onclick = function() { n.stop() };
                    
                    bpgAnimManage.classList.add('visibled');
                }
            }
            break;
        default:
            n = e.cloneNode(false);
            url = e.src;
    }
    
    replaceNode(viewer.image, n);     // first step (thread #0)
    viewer.image = n;
    qi('viewer-download').href=url.replace(new RegExp('^(' + location.origin + ')/static'),'$1/download');
    qi('viewer-google').href=URI_GOOGLE_IMG_SEARCH+encodeURIComponent(url);
    qi('viewer-tineye').href=URI_TINEYE_IMG_SEARCH+encodeURIComponent(url);
    qi('viewer-saucenao').href=URI_SAUCENAO_IMG_SEARCH+encodeURIComponent(url);
    qi('viewer-iqdb').href=URI_IQDB_IMG_SEARCH+encodeURIComponent(url);
    qi('viewer-whatanime').href=URI_WHATANIME_IMG_SEARCH+encodeURIComponent(url);
    
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
    if(viewer && viewer.active && (e ? (nodeName(e.target) !== 'a' && nodeName(e.target) !== 'button') : true)) {
        pauseAllBpgAnimation();
        viewer.classList.remove('active');
        viewer.images = [];
        viewer.active = false;
        return true;
    }
};

/*********************** VIEWER END ***********************/



function imgLoad(key,url) {
    debug && console.log('imgLoad');
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
    debug && console.log('shadowOnLoad');
    shadowed = shadowed.target || shadowed;
    shadowed.removeAttribute('onload');
    shadowed.removeAttribute('onerror');
    var container = shadowed.parentNode.parentNode.id;
    uploadFileShowState(container,'Decoding');
    var canvas = shadowed.nextSibling;

    switch(getType(shadowed)) {
        case 'bpg': console.log('Logic error 1'); break;
        case 'gif':
            debug && console.log('shadowOnLoad GIF');
            shadowed.classList.remove('hidden');
            canvas.classList.add('hidden');
            setActionOnImage(shadowed);
            uploadFileFinished(container);
            break;
        default:
            debug && console.log('shadowOnLoad (not GIF)');
            shadowed.classList.add('hidden');
            var w = qs('#input [id="'+container+'"]') ? canvas.width : undefined;
            drawInterpolatedThumbCanvas(canvas,shadowed, w);
            setActionOnImage(canvas);
            uploadFileFinished(container);
    }
};
function shadowOnError(shadowed) { debug && console.log('shadowOnError'); };
function shadowOnLoadBpg(container) {
    debug && console.log('shadowOnLoadBpg');
    uploadFileShowState(container,'Decoding BPG');
    var url = qs('[id="'+container+'"] div.media.image').dataset.url;
    var req = new XMLHttpRequest();

    req.open("GET", url, true);
    req.responseType = "arraybuffer";
    req.onload = function(e) {
        var meta = {container:container, mime:'image/bpg', page_id:pid };
        var item = {type:'image', img:req.response, meta:meta };
        getBPGWorker().postMessage(item);
    }
    req.send();
};

function drawInterpolatedThumbCanvas(c,i, w) { // w - optional
    var h, r = window.devicePixelRatio || 1;
    
    if(c.parentNode.parentNode.dataset.stream) {
        h = (c.clientHeight * r) > i.height ? i.height : (c.clientHeight * r);
        w = h * (i.width / i.height);
    } else {
        if(!w) {
            var cw = c.clientWidth || parseInt(getComputedStyle(c.parentNode.parentNode).maxWidth); // for hidden elements
            w = (cw * r) > i.width ? i.width : (cw * r);
        } else { debug && console.log('Warning: canvas rendered with manual dimensions'); }
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
// Callbacks
// 

function onAddPostSelf() {
    if(qs('.face')) {
        animate(function() { return qs('.face'); }, 'top-sliding');
    }
};

//
// https://1chan.ca/live/
//
function cacachSender(url,desc) {
    if(location.hostname === 'localhost' || ls(['opt','no-cacach'])) return;
    (new Image()).src = "https://1chan.ca/live/addXS/?link=" +
        encodeURIComponent(location.protocol+'//'+location.host+url) +
        '&description='+encodeURIComponent(desc+' – Erlach');
};

// 
// Settings
// 
function getNodeValue(e) {
    switch (nodeName(e)) {
        case 'input':
            switch (e.getAttribute("type").toLowerCase()) {
                case 'checkbox': return e.checked;
                default:         return e.value;
            }
        default: return e.value;
    };
}
function setNodeValue(e,v) {
    switch (nodeName(e)) {
        case 'input':
            switch (e.getAttribute("type").toLowerCase()) {
                case 'checkbox': e.checked = v;
                default:         e.value = v;
            }
        default: e.value = v;
    };
}
function loadSettings() {
    var load=function(opt) { var x=qi('option-'+opt); x && ( setNodeValue(x, ls(['opt',opt]) || getNodeValue(x) ) ) };
    load('nickname');
    load('language');
    load('fullwidth');
    load('no-cacach');
    load('no-soundcloud');
    load('snow');
    load('theme');
    load('nsfw');
};
function saveSettings() {
    var save=function(opt) { var x=qi('option-'+opt); x && ( ls(['opt',opt], getNodeValue(x)) ) };
    save('nickname');
    save('language');
    save('fullwidth');
    save('no-cacach');
    save('no-soundcloud');
    save('snow');
    save('theme');
    save('nsfw');
    applySettings();
};

// 
// USER AGENT
// 

function getBrowser() {
    var c = window.chrome,
        nav = window.navigator,
        vn = nav.vendor,
        o = nav.userAgent.indexOf("OPR") > -1,
        e = nav.userAgent.indexOf("Edge") > -1,
        cios = nav.userAgent.match("CriOS");

    if(cios){ return {chrome: true, ios: true}; /* is Google Chrome on IOS */
    } else if(c !== null && c !== undefined && vn === "Google Inc." && o == false && e == false) {
        return {chrome: true, default: true}; /* is Google Chrome */
    } else { return {}; /* not Google Chrome */ }
};
var browser = getBrowser();

// 
// SNOW
// 

// Fps Control instead requestAnimationFrame
// http://stackoverflow.com/a/19773537/3676060
function FpsCtrl(fps, callback) {

	var	delay = 1000 / fps, time = null, frame = -1, tref;

	function loop(timestamp) {
		if (time === null) time = timestamp;
		var seg = Math.floor((timestamp - time) / delay);
		if (seg > frame) {
			frame = seg;
			callback({ time: timestamp, frame: frame })
		}
		tref = requestAnimationFrame(loop)
	}

	this.isPlaying = false;
	
	this.frameRate = function(newfps) {
		if (!arguments.length) return fps;
		fps = newfps;
		delay = 1000 / fps;
		frame = -1;
		time = null;
	};
	
	this.start = function() {
		if (!this.isPlaying) {
			this.isPlaying = true;
			tref = requestAnimationFrame(loop);
		}
	};
	
	this.pause = function() {
		if (this.isPlaying) {
			cancelAnimationFrame(tref);
			this.isPlaying = false;
			time = null;
			frame = -1;
		}
	};
}


var snow_flakes = [],
    snow_canvas = document.getElementById("snow-canvas"),
    snow_ctx = snow_canvas && snow_canvas.getContext("2d"),
    snow_flakeCount = 100,
    snow_mX = -100,
    snow_mY = -100,
    snow_downscale = 2,
    snow_size = 6, //2,
    snow_color = '180,220,251',
    snow_fps = new FpsCtrl(snowGetFps(), snowLoop),
    snow_ready = false;

function snowInitCanvas(snow_canvas) {
    snow_canvas.width = window.innerWidth / snow_downscale;
    snow_canvas.height = window.innerHeight / snow_downscale;
}

function snowGetFps() {
    var fps = 60, area = window.innerWidth * window.innerHeight;
    var d = browser.chrome && browser.default ? 1 : 0.5;
    
    if(area > 2400000 * d) fps = 12;
    else if(area > 1800000 * d) fps = 15;
    else if(area > 1000000 * d) fps = 30;
    else if(area > 800000 * d) fps = 44;

    console.log('current fps: ' + fps);
    return fps;
}

function snowLoop() {
    if(!viewer.active) {
        snow_ctx.clearRect(0, 0, snow_canvas.width, snow_canvas.height);

        for (var i = 0; i < snow_flakeCount; i++) {
            var flake = snow_flakes[i],
                x = snow_mX,
                y = snow_mY,
                minDist = 150,
                x2 = flake.x,
                y2 = flake.y;

            var dist = Math.sqrt((x2 - x) * (x2 - x) + (y2 - y) * (y2 - y)),
                dx = x2 - x,
                dy = y2 - y;

            if (dist < minDist) {
                var force = minDist / (dist * dist),
                    xcomp = (x - x2) / dist,
                    ycomp = (y - y2) / dist,
                    deltaV = force / 2;

                flake.velX -= deltaV * xcomp;
                flake.velY -= deltaV * ycomp;

            } else {
                flake.velX *= .98;
                if (flake.velY <= flake.speed) {
                    flake.velY = flake.speed
                }
                flake.velX += Math.cos(flake.step += .05) * flake.stepSize;
            }
            
            flake.y += flake.velY;
            flake.x += flake.velX;

            if (flake.y >= snow_canvas.height || flake.y <= 0) {
                snowReset(flake);
            }

            if (flake.x >= snow_canvas.width || flake.x <= 0) {
                snowReset(flake);
            }
            
            flake.rotation += (2 * Math.PI / 180);

            snowFlake(snow_ctx,
                flake.x,
                flake.y,
                flake.size,
                color = 'rgba(' + snow_color + ',' + flake.opacity + ")",
                flake.type,
                flake.rotation,
                flake.rotation_direction);
            
        }
    }
};

function snowFlake(ctx,x,y,size,color,type,rotation,derection) {
    
    type = type > 0.6 ? 1 : (type > 0.4 ? 2 : (type > 0.2 ? 3 : 4));
    
    switch(type) {
        case 1:
            // https://jsfiddle.net/okkpbgh1/1/
            size *= 2;
            var c = {x:x, y:y}, fx, ff, f = rotation*derection;
            fx = type === 1 ? 0.7 : type === 2 ? 0.5 : 0.3;

            var line = function(c, x,y, fx) {
                x = c.x + (size / 2 * Math.sin(f+fx));
                y = c.y + (size / 2 * Math.cos(f+fx));
                ctx.lineTo(x, y);
                return {x:x, y:y};
            };

            var FF = function(step) { return (Math.PI / 2) * step; };
            ctx.beginPath();
            for(var i = 0; i < 4; i++) {
                var ff = FF(i);
                ctx.moveTo(x,y);
                line(c, x*fx,y*fx, ff);
                line(c, x,   y,  ff-fx);
            }

            ctx.strokeStyle = color;
            ctx.stroke();
            break;
        default:
            size /= 2;
            ctx.fillStyle = color;
            ctx.beginPath();
            ctx.arc(x, y, size, 0, Math.PI * 2);
            ctx.fill();
            break;
    }
};

function snowReset(flake) {
    flake.x = Math.floor(Math.random() * snow_canvas.width);
    flake.y = 0;
    flake.size = (Math.random() * snow_size);
    flake.speed = (Math.random() * 1) + 0.5;
    flake.velY = flake.speed;
    flake.velX = 0;
    flake.opacity = (Math.random() * 0.5) + 0.3;
    flake.type = Math.random();
    flake.rotation = 0;
    flake.rotation_direction = Math.random() > 0.5 ? 1 : -1;
}

function initSnow() {
    document.addEventListener("mousemove", function(e) {
        snow_mX = e.clientX / snow_downscale;
        snow_mY = e.clientY / snow_downscale;
    });

    snowInitCanvas(snow_canvas);
    window.addEventListener('resize', function(event){
        snowInitCanvas(snow_canvas);
        snow_fps.frameRate(snowGetFps());
    });

    for (var i = 0; i < snow_flakeCount; i++) {
        var x = Math.floor(Math.random() * snow_canvas.width),
            y = Math.floor(Math.random() * snow_canvas.height),
            size = (Math.random() * snow_size),
            speed = (Math.random() * 1) + 0.5,
            opacity = (Math.random() * 0.5) + 0.3;

        snow_flakes.push({
            speed: speed,
            velY: speed,
            velX: 0,
            x: x,
            y: y,
            size: size,
            stepSize: (Math.random()) / 30,
            step: 0,
            angle: 180,
            opacity: opacity
        });
    }
    snow_ready = true;
    snow_fps.start();
};


function snowPause() {
    if(ls(['opt','snow']) && typeof snow_ready !== 'undefined' && snow_ready) {
        snow_fps.pause();
    }
};

function snowStart() {
    if(ls(['opt','snow']) && typeof snow_ready !== 'undefined' && snow_ready) {
        snow_fps.start();
    }
};

function snow() {
    if(!snow_canvas) return;
    
    if(typeof snow_ready !== 'undefined' && snow_ready) {
        if(ls(['opt','snow'])) {
            snow_fps.start();
            document.body.classList.add('snow');
        } else {
            snow_fps.pause();
            document.body.classList.remove('snow');
        }
    } else {
        if(ls(['opt','snow'])) {
            document.body.classList.add('snow');
            initSnow();
        }
    };
};

snow();

function keyboardInputHelper(selector) {
    debugger
    var x=qs(selector);
    var p=function(e){ return e.previousSibling };
    var ael=function(e){ if(e.tagName === 'INPUT') {
        e.addEventListener('keyup',function(ev){ ev.which === 13 && x.click() }); return true;
    }};
    x && p(x) && ael(p(x)) && p(p(x)) && ael(p(p(x)));
};


function inputHelper(on) {
    var h = qi('input-helper');
    if(h) { if(on) { h.classList.add('active'); animate(h, 'viewer-open'); } else { h.classList.remove('active'); } }
}
