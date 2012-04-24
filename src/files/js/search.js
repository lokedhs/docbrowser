function ajaxRequest( url, data, callback ) {
    console.log( "Sending AJAX request to: " + url + ", data: " + JSON.stringify( data ) );
    $.ajax( {
        type: "POST",
        url: url,
        data: JSON.stringify( data ),
        dataType: "text",
        contentType: "application/x-msgpack",
        processData: false,
        success: function( xml ) {
            console.log( "got response from ajax call. url=" + url + ", data=" + xml );
            callback( xml );
        }
    } );
}

function search( text ) {
    var data = { text: text };
    ajaxRequest( "/search_command", data, function( xml ) {
        console.log( "got result: " + xml );
    } );
}

function init() {
    $("#searchText").change( function() {
        search( $(this).prop( "value" ) );
    } );
}

$(document).ready( init );
