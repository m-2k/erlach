-ifndef(SPA_ELEMENTS_HRL).
-define(SPA_ELEMENTS_HRL, true).

-include_lib("nitro/include/nitro.hrl").
-record(a,  {?ELEMENT_BASE(element_a),  href, hreflang, media, rel, target, type, url="javascript:void(0);", download, name}).

-endif.
