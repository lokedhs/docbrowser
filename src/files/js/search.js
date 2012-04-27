function ajaxRequest( url, data, callback ) {
    $.ajax( {
        type: "POST",
        url: url,
        data: JSON.stringify( data ),
        dataType: "text",
        contentType: "application/x-msgpack",
        processData: false,
        success: function( xml ) {
            callback( xml );
        }
    } );
}

function handleSearchResult( data ) {
    var tableBody = $("#results > tbody");
    tableBody.find( "tr" ).remove();
    for( var i = 0 ; i < data.length ; i++ ) {
        var entry = data[i];
        var row = $("<tr>");
        row.append( $("<td>").append( entry.name ) );
        row.append( $("<td>").append( entry.type ) );
        tableBody.append( row );
    }
}

var inProgress = false;
var pending = false;

function search( text ) {
    if( inProgress ) {
        pending = true;
    }
    inProgress = true;

    var data = { text: text };
    ajaxRequest( "/search_command", data, function( xml ) {
        inProgress = false;
        if( pending ) {
            pending = false;
            callback();
        }
        else {
            handleSearchResult( JSON.parse( xml ) );
        }
    } );
}

function init() {
    var callback = function() {
        search( $("#searchText").attr( "value" ) );
    };
    $("#searchText").change( callback ).keyup( callback );
}

$(document).ready( init );
