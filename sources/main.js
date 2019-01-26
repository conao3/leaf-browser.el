$(document).ready(function() {
    // $(".dropdown-button").dropdown();
    // $('.sidenav').sidenav();
    // $('.collapsible').collapsible();
    // $('.modal').modal();
    // $('.tabs').tabs();
    // $('.tooltipped').tooltip();
    // $('.scrollspy').scrollSpy();
    M.AutoInit();

    $('#play-click').on('click', function() {
        alert("play-click");
    });

    $('#play-ajax').on('click', function() {
        $.ajax({
            type: "POST",
            url: "/leaf-browser/ajax",
            data: //"name=John&location=Boston",
                  {
                      'userid': 's',
                      'username': 'a'
                  }
        })
         .done ((data) => {
             // alert( "Data Saved: " + data );
         })
         .always ((data) => {
             $('#pre-play-ajax').html(data);
         });
    });
});

function formSubmit () {
    document.forms['form-main'].submit()
}

function formSubmitWithCopy () {
    document.forms['form-main'].submit()
}
