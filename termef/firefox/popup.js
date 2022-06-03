// Build a breadcrumbs-like list of ancestors in the ontology
const ancestors = ( arr ) => {
    let str = "";
    for( elt in arr ){
	str += arr[elt]['name'];
	str += " > ";
    }
    return str.slice(0,-3);
};

// Format the TerMef results as an HTML nested list for `jsTree'
const infobox = ( data ) => {
    let node;
    let resp = '<ul>';
    for( node in data['items'] ){
	let v = data['items'][node]['attributes'].find( x => x['attribute'] == "groupe" );
	if( undefined != v){
	    // Path in ontology 
	    resp += "<li>" + ancestors( v['values'][0] ) + '<ul>';
	    
	    // Subitem `Term'
	    v = data['items'][node]['attributes'].find( x => x['attribute'] == "Nom" );
	    if( undefined != v ){
		resp += "<li><a href=\">" + data['items'][node]['url'] + "\">" + "Terme : <b>" + v['values'][0] + "</b></a></li>";
	    }

	    // Subitem `Definition'
	    v = data['items'][node]['attributes'].find( x => x['attribute'] == "d\xE9finition" );
	    if( undefined != v ){
		resp += "<li>D\xE9finition : " + v['values'][0] +'</li>';
	    }
	    
	    // Subitem `Notes', as a subtree
	    v = data['items'][node]['attributes'].find( x => x['attribute'] == "note" );
	    if( undefined != v ){
		resp += "<li>Notes<ul>";
		for( subnode in v['values'] ){
		    resp += "<li>" + v['values'][subnode] + "</li>";
		}
		resp += '</ul></li>';
	    }
	    
	    // Subitem `Publication date'
	    v = data['items'][node]['attributes'].find( x => x['attribute'] == "date de parution au JO" );
	    if( undefined != v ){
		resp += "<li>JO du " + v['values'][0] + ".</li>";
	    }
	    resp += '</ul></li>';
	}
    }
    resp += '</ul>';
    jQuery('#jstree_demo_div').jstree('destroy');
    jQuery('#jstree_demo_div').empty();
    jQuery('#jstree_demo_div').append(resp);
    jQuery(function () {
	jQuery('#jstree_demo_div').jstree();
	    // .bind("select_node.jstree", function (e, data) {
	    // 	let href = data.node.a_attr.href;
	    // 	console.log( href );
	    // 	document.location.href = href;
	    // });
    });

    return resp;
};

// Attach `enter' key to query execution in the input field
jQuery( '#qtext' ).on( 'keypress', function(e) {
    if(e.which == 13) {
        $('#query').trigger('click');
    }
});

// When the button is clicked, get the term infobox
jQuery('#query').click( async () => {
    let [tab] = await browser.tabs.query({ active: true, currentWindow: true });
    let color = jQuery('#qtext').val();
    let query = '?q=' + color + '&s=1&mode=exact&field=label';
    jQuery.ajax( "https://terminologie.finances.gouv.fr/search/infobox" + query,
		 {
		     success: (data, textStatus, jqXHR ) => {
			 let resp = infobox( data );
			 // jQuery('#infobox').val( resp );
		     },
		     error: ( jqXHR, textStatus, errorThrown ) => {
			 // jQuery('#infobox').val( "TerMef inaccessible!" );
		     }
		 });
});
