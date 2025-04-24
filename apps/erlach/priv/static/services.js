// 
// Erlach Imageboard Services (htpps://erlach.co/about)
// 
var erlach = {
    debug: false,
    log: function(o,raw) { erlach.debug && console.log(raw ? o : '[erlach] ' + o) },
    error: function(o,raw) { console.error(raw ? o : '[erlach] ' + o) }
};

(function() { var requestAnimationFrame = window.requestAnimationFrame || window.mozRequestAnimationFrame ||
    window.webkitRequestAnimationFrame || window.msRequestAnimationFrame; })();

window.addEventListener('message', function(e){
    var d = e.data;
    if(d) {
        switch(d.action) {
            case 'resize':
                var frame = document.querySelector('iframe[src="'+d.location+'"]')
                if(frame) {
                    requestAnimationFrame((function() {
                        frame.style.height = parseInt(d.height) + 'px';
                        frame.style.display = 'block';
                        erlach.log('resized');
                    }).bind(this));
                }
                else { erlach.error('frame with location ' + d.location + ' not found') }
                break;
            default: erlach.log('unknown event'); erlach.log(d,true);
        }
    } else { erlach.error('wrong data received') };
    
},false);