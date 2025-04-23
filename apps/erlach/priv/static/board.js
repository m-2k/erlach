/* DRUG AND DROP VISUAL EFFECTS START */

var thumbnails = {};
var thumb_iterator = 0;

$binary.do = function(e) { debugger; console.log("BINARY: " + e) };
$bert.do = function(e) { console.log("BERT: " + e) };


function drag_input_init() {
    var ipt = document.getElementById('drag-input-form');
    if(ipt) {
        var resetTimer;
        var className = 'over';

        var resetUploadForm = function() {
            var f = function() { ipt.classList.remove(className); };
            resetTimer = window.setTimeout(f, 50);
        }
    
        document.body.addEventListener("dragover", function(e) {
            e.stopPropagation();
            e.preventDefault();
            if (resetTimer) { clearTimeout(resetTimer); }
            ipt.classList.add(className);
        }, true);
        document.body.addEventListener("dragleave", resetUploadForm, true);
        document.body.addEventListener("drop", resetUploadForm, true);  // For Body
        ipt.addEventListener("drop", function(e) {                      // For Form (overloaded)
          e.stopPropagation();
          e.preventDefault();
          resetUploadForm();
          handleFileSelect(e);
        }, false);
    };
};

drag_input_init();

function fileLoadFinished(pos) {
	thumbnails[pos].style.opacity = 1.0;
	thumbnails[pos].is_loaded = true;
};
function fileLoadFailed(pos) {
    console.log("Upload error" + pos);
    thumbnails[pos].style.opacity = 0.2;
};

function roundedCanvas(ctx,x,y,width,height,radius){
      ctx.beginPath();
      ctx.moveTo(x + radius, y);
      ctx.lineTo(x + width - radius, y);
      ctx.quadraticCurveTo(x + width, y, x + width, y + radius);
      ctx.lineTo(x + width, y + height - radius);
      ctx.quadraticCurveTo(x + width, y + height, x + width - radius, y + height);
      ctx.lineTo(x + radius, y + height);
      ctx.quadraticCurveTo(x, y + height, x, y + height - radius);
      ctx.lineTo(x, y + radius);
      ctx.quadraticCurveTo(x, y, x + radius, y);
      ctx.closePath();
    };

/// http://stackoverflow.com/questions/26288376/alternative-to-base64-when-loading-data-from-file-input
function handleFileSelect(evt) {
  var files = evt.dataTransfer.files;
  var output = [];
  
  for (var i = 0, file; file = files[i]; i++) {
		if (!file.type.match('^(image/jpeg)|(image/png)|(image/gif)$')) { console.log("Ignoring file type: " + file.type); continue; }
		else { console.log("Accepted file type: " + file.type); };
        
        /// Canvas HD
        var h = 35;
        var newimg = document.createElement("img");
        newimg.addEventListener('load', (function () {
            var newimg_container = document.createElement("div");
            var canvas = document.createElement("canvas");
            var ctx = canvas.getContext('2d');
            newimg_container.setAttribute("class", "thumb-container");
            var w = h * (this.image.width / this.image.height);
            newimg_container.style.width = Math.floor(w);
            
            var cw, ch, crop;
            if( w < h ) {
                cw = h; ch = h; crop = (this.image.height - this.image.width) / 2;
            } else { cw = w; ch = h; crop = 0; }
            
            /// finally query the various pixel ratios
            var devicePixelRatio = window.devicePixelRatio || 1;
            var backingStoreRatio = ctx.backingStorePixelRatio || 1;

            var ratio = (devicePixelRatio / backingStoreRatio) * 2;
            /// upscale the canvas if the two ratios don't match
            // if (devicePixelRatio !== backingStoreRatio) {
                canvas.width = cw * ratio;
                canvas.height = ch * ratio;
                canvas.style.width = cw + 'px';
                canvas.style.height = ch + 'px';
                ctx.scale(ratio, ratio);
                console.log("Using ratio: " + ratio + " (" + canvas.width + "x" + canvas.height + ")");
            // }
            
            ctx.save();
            roundedCanvas(ctx,0,0,cw,ch,2);
            ctx.clip();
            ctx.drawImage(this.image, 0, crop, this.image.width, this.image.height-crop-crop , 0, 0, cw, ch);
            ctx.restore();
            
            /// Removing attachment
            /// c-ontainer, p-osition, n-ame
            newimg_container.addEventListener('click', (function() {
                
                this.container.parentNode.removeChild(this.container);
                window.URL.revokeObjectURL(this.blob_url);
                ws.send(enc(tuple(atom('bin'),tuple(atom('attachment'), atom('remove'), this.position))));
                console.log("Sended event to remove attach: " + this.position + " :: " + this.filename);
                
            }).bind({blob_url: this.image.src,
                container: newimg_container,
                position: thumb_iterator,
                filename: this.file.name}));

			canvas.style.opacity = 0.4;
			thumbnails[thumb_iterator] = canvas;
            newimg_container.appendChild(canvas);
            document.getElementById('thumbnail-list').insertBefore(newimg_container, null);
            console.log("New Blob: " + this.image.src);
            
            /// Sending attachment
            /// r-esult, p-osition, n-ame, t-ype, s-ize, d-ate
            var reader_blob = new FileReader();
            reader_blob.addEventListener("loadend", (function() {
                // SEND TO SERVER
				console.log("Sending attachment");
                ws.send(enc(tuple(atom('bin'),
                    tuple(atom('attachment'),atom('upload'),
                    this.p, this.n, this.t,
                    this.s, this.d, bin(this.r.result)))));
            }).bind({r: reader_blob,
                p: thumb_iterator,
                n: this.file.name,
                t: this.file.type,
                s: this.file.size,
                d: this.file.lastModifiedDate}));
            // reader_blob.readAsArrayBuffer(file);
            reader_blob.readAsBinaryString(this.file);
            
            thumb_iterator++;
            
        }).bind({image: newimg, file: file}));
        newimg.src = window.URL.createObjectURL(file);
    }
}

/* DRUG AND DROP VISUAL EFFECTS END */

/* TEXTAREA VERTICAL AUTORESIZE START */

function textarea_height_calc(e) {
    e.style.height = "auto";
    e.style.overflowY = "hidden";
    e.style.height = e.scrollHeight;
}
function textarea_init(t) {
    textarea_height_calc(t);
    t.addEventListener('input', function () { textarea_height_calc(this) } );
    if(t.classList.contains('autostore')) { textarea_init_autostore(t); };
};
var ta_elements = document.querySelectorAll('textarea');
Array.prototype.forEach.call(ta_elements, function(t, i){ textarea_init(t) });

/* TEXTAREA VERTICAL AUTORESIZE END */

/* TEXTAREA SHADOW PROCESSES START */

var STORE_TIMEOUT = 4000;
var storeTimer;
function textarea_init_autostore(t) {
    var onchange = function() {
    
        /// Enabling button
        // send_message.disabled = textarea.value === "" ? true : false;
    
        /// Shadow text storing
        window.clearTimeout(storeTimer);
        if(t.value !== "") {
            storeTimer = window.setTimeout(function(){
                console.log("Auto storing");
                ws.send(enc(tuple(atom('client'),atom('auto_store'))));
            },STORE_TIMEOUT);
        }
    
    
    }
    t.addEventListener('change', onchange);
    t.addEventListener('keyup', onchange);
};

function publish_finished() {
    window.clearTimeout(storeTimer);
    qi('message').value="";
    
    thumbnails = {};
    thumb_iterator = 0;
    
    thumbnail_list = document.getElementById('thumbnail-list')
    while (thumbnail_list.firstChild) {
      thumbnail_list.removeChild(thumbnail_list.firstChild);
    };
};

/* TEXTAREA SHADOW PROCESSES END */

/* IMAGE VIEWER START */

var im_elements;

function image_click_event(){
    im_f.src = this.src;
    im_f.idx = im_elements.indexOf(this);
    im_f.style.webkitAnimationName = im_f.style.animationName = "flash";
    im_b.style.opacity = "0";
    im_b.src = undefined;
    ivo.style.webkitAnimationName = ivo.style.animationName = "slide-right";
    start_keyframes_timer();
}

function dom_image_list_changed(){
	if(ivo) {
	    im_elements = Array.prototype.slice.call(document.querySelectorAll('.post-attachment .image'));
	    Array.prototype.forEach.call(im_elements, function(el, i){
		  el.removeEventListener('click', image_click_event, false);
	      el.addEventListener('click', image_click_event, false);
	    });
	};
};

var ivo = document.getElementById('image-viewer-overlay');
if(ivo) {
    var im_b = document.getElementById('image-viewer-picture-back');
    var im_f = document.getElementById('image-viewer-picture-front');
    im_b.idx = im_f.idx = undefined;
    var reverse = false;
    var KEYFRAMES_TIMEOUT = 200;
    var keyframesTimer;
    
    dom_image_list_changed();

    ivo.addEventListener('click', function (evt) { e = evt || window.event; if(e.target == this) { close_image_viewer(); } });
    im_f.addEventListener('click', function (evt) { e = evt || window.event; if(e.target == this) { next_image(); } });
    im_b.addEventListener('click', function (evt) { e = evt || window.event; if(e.target == this) { next_image(); } });

    function close_image_viewer() {
        keyframesTimer = undefined;
        ivo.style.webkitAnimationName = ivo.style.animationName = "slide-left";
        reverse = false;
        start_keyframes_timer();
    };
    function start_keyframes_timer() { keyframesTimer = window.setTimeout( function() { keyframesTimer = undefined;  },KEYFRAMES_TIMEOUT) };
    
    function next_index(idx) { return ++idx >= im_elements.length ? 0 : idx };
    function prev_index(idx) { return --idx < 0 ? im_elements.length - 1 : idx };
    function next_image() { change_image(true) };
    function prev_image() { change_image(false) };
    function change_image(direction) {
        if(im_elements.length <= 1) {
            close_image_viewer();
        } else {
            if(keyframesTimer === undefined) {
                var im1 = reverse ? im_b : im_f;
                var im2 = reverse ? im_f : im_b;
                reverse = !reverse;

                im2.idx = direction ? next_index(im1.idx) : prev_index(im1.idx);
                im2.src = im_elements[im2.idx].src;

                if(im1.src) { im1.style.webkitAnimationName = im1.style.animationName = direction ? "slide-left" : "hide"; }
                im2.style.webkitAnimationName = im2.style.animationName = direction ? "flash" : "slide-right";
                start_keyframes_timer();
            }
        }
    };
    document.addEventListener('keydown', function(evt) {
        e = evt || window.event;
        switch (e.keyCode) {
            case 37: prev_image(); break; // left
            case 39: next_image(); break; // right
            case 27: close_image_viewer(); break; // esc
        }
    });
};

/* IMAGE VIEWER END */