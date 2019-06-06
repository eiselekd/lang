
            $.getJSON( "ajax/test.json", function( data ) {
                var items = [];
                $.each( data, function( key, val ) {
                    items.push( "<li id='" + key + "'>" + val + "</li>" );
                });

                $( "<ul/>", {
                    "class": "my-new-list",
                    html: items.join( "" )
                }).appendTo( "body" );

                var rows = $(html).filter("tr");
                rows.find(".directory").parents("tr").each(function() {
                    //droppableSetup.apply(this);
                });
                table.treetable("loadBranch", node, rows);


            });
