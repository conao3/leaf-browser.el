$(document).ready(function() {
    // $(".dropdown-button").dropdown();
    // $('.sidenav').sidenav();
    // $('.collapsible').collapsible();
    // $('.modal').modal();
    // $('.tabs').tabs();
    // $('.tooltipped').tooltip();
    // $('.scrollspy').scrollSpy();
    M.AutoInit();

    $('#play-click').on('click',function(){
        alert("play-click");
    });
});

function formSubmit () {
    document.forms['form-main'].submit()
}

function formSubmitWithCopy () {
    document.forms['form-main'].submit()
}
