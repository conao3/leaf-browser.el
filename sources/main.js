$(document).ready(function() {
    // $(".dropdown-button").dropdown();
    // $('.sidenav').sidenav();
    // $('.collapsible').collapsible();
    // $('.modal').modal();
    // $('.tabs').tabs();
    // $('.tooltipped').tooltip();
    // $('.scrollspy').scrollSpy();
    M.AutoInit();
});

if (!!window.performance && window.performance.navigation.type == 2) {
    window.location.reload();
}

function formSubmit () {
    document.forms['form-main'].submit()
}

function formSubmitWithCopy () {
    document.forms['form-main'].submit()
}
