{application,db,
             [{description,"Custom Persistent Storage"},
              {vsn,"1"},
              {registered,[]},
              {applications,[kernel,stdlib,kvs]},
              {mod,{db_app,[]}},
              {env,[]},
              {modules,[db_app,db_attachment,db_board,db_post,db_sup,
                        db_thread,db_token,db_user]}]}.
