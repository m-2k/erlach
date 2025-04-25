-define(SUP_CLASS,converter).


 % port_map: #{#Port<0.46018> => "2vvbdE", … }
 % entry_map: #{"2vvbdE" => {entry,"2vvbdE", … }, … }
-record(state,{queue = [], entry_map = #{}, port_map = #{}}).
-record(entry,{
    id, group, meta, source, destination, target, finally, error, autostart=true, % user defined
    storage, path, port, from, infoA, infoB, stage, req                           % private
}).

-define(IMAGE_INFO, width, height, size = 0, alpha = false, animation = false, bit_depth = 8, color_space).
-record(image_info, { ?IMAGE_INFO }).
-record(jpeg, { ?IMAGE_INFO }).
-record(png, { ?IMAGE_INFO, color_type }).
-record(gif, { ?IMAGE_INFO, frames, alpha_frames = 0, delays = [], loop_count = 0 }).
-record(bpg, { ?IMAGE_INFO, pixel_format, extension_present, limited_range }).