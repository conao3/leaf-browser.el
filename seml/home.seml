'(html nil
   (head nil
     (link ((rel . "stylesheet") (href . "https://fonts.googleapis.com/icon?family=Material+Icons")))
     (link ((rel . "stylesheet") (href . "https://fonts.googleapis.com/css?family=Source+Sans+Pro")))
     (link ((rel . "stylesheet") (href . "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css")))
     (script ((src . "https://code.jquery.com/jquery-2.1.1.min.js")))
     (script ((src . "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/js/materialize.min.js")))
     (meta ((charset . "utf-8")))
     (meta ((name . "viewport") (content . "width=device-width, initial-scale=1.0")))
     (title nil "home")
     (style ((type . "text/css"))
       ".container { font-family: 'Source Sans Pro', sans-serif; }")
     (script ((type . "text/javascript"))
       "
//M.AutoInit();
$(\".dropdown-button\").dropdown();
$(document).ready(function(){
  $('.sidenav').sidenav();
});

$(document).ready(function(){
  $('.collapsible').collapsible();
});

$(document).ready(function(){
  $('.modal').modal();
});

$(document).ready(function(){
  $('.tabs').tabs();
});

$(document).ready(function(){
  $('.tooltipped').tooltip();
});
"))
   (body ((class . "grey darken-4 grey-text text-lighten-5"))
     (header nil
       (nav ((class . "nav-extended indigo darken-4"))
         (div ((class . "container"))
           (div ((class . "nav-wrapper row"))
             (div ((class . "col s6"))
               (h4 nil (a ((href . "#") (class . "center")) "Leaf-browser.el")))
             (div ((class . "col s12"))
               (div ((class . "center"))
                 (img ((class . "responsive-img") (src . "/leaf-browser/sources/splash.svg")))))
             (ul ((class . "right hide-on-med-and-down"))
               (li nil (a ((href . "sass.html")) "Sass"))
               (li nil (a ((href . "badges.html")) "Components"))
               (li nil (a ((href . "collapsible.html")) "JavaScript"))))
           (div ((class . "nav-content"))
             (ul ((class . "tabs tabs-transparent"))
               (li ((class . "tab"))
                 (a ((href . "#test1") (class . "active")) "test1"))
               (li ((class . "tab"))
                 (a ((href . "#test2")) "test2"))
               (li ((class . "tab"))
                 (a ((href . "#test3")) "test3")))
             (span ((class . "nav-title")) "Title")
             (a ((class . "btn-floating btn-large halfway-fab waves-effect waves-light teal"))
               (i ((class . "material-icons")) "add"))))))
     (div ((class . "container"))
       (a ((onclick . "M.toast({html: 'I am a toast'})") (class . "btn")) "Toast!")
       (a ((class . "btn tooltipped") (data-position . "top") (data-tooltip . "I am a tooltip")) "Hover me!")

       (div ((class . "switch")) (input ((type . "checkbox"))))
       
       (a ((href . "#") (data-target . "slide-out") (class . "sidenav-trigger"))
         (i ((class . "material-icons")) "menu"))
       (ul ((id . "slide-out") (class . "sidenav"))
         (li nil
           (div ((class . "user-view"))
             (div ((class . "background"))
               (img ((src . "images/office.jpg"))))
             (a ((href . "#user"))
               (img ((class . "circle") (src . "images/yuna.jpg"))))
             (a ((href . "#name"))
               (span ((class . "white-text name")) "John Doe"))
             (a ((href . "#email"))
               (span ((class . "white-text email")) "jdandturk@gmail.com"))))
         (li nil (a ((href . "#!")) (i ((class . "material-icons")) "cloud") "First Link With Icon"))
         (li nil (a ((href . "#!")) "Second Link"))
         (li nil (div ((class . "divider"))))
         (li nil (a ((class . "subheader")) "Subheader"))
         (li nil
           (a ((class . "waves-effect") (href . "#!")) "Third Link With Waves")))

         (div ((id . "modal1") (class . "modal"))
         (div ((class . "modal-content"))
           (h4 nil "Modal Header")
           (p nil "A bunch of text"))
         (div ((class . "modal-footer")) (a ((href . "#!") (class . "modal-close waves-effect waves-green btn-flat")) "Agree")))
       (button ((data-target . "modal1") (class . "btn modal-trigger")) "Modal")
       
       (button ((class . "btn waves-effect waves-light") (type . "submit") (name . "action"))
         "Submit" (i ((class . "material-icons right")) "send"))
       (button ((class . "btn waves-effect waves-light") (type . "submit") (name . "action"))
         "Copy" (i ((class . "material-icons right")) "content_copy"))
       (button ((class . "btn waves-effect waves-light") (type . "submit") (name . "action"))
         "Cloud" (i ((class . "material-icons right")) "backup"))
       
       (div ((class . "center-align"))
         (img ((class . "responsive-img") (src . "/leaf-browser/sources/splash.svg"))))
       (div ((class . "row"))
         (div ((class . "col s3"))
           (comment nil "Grey navigation panel"))
         (div ((class . "col s9"))
           (comment nil "Teal page content"))))
     (footer ((class . "page-footer"))
       (div ((class . "container"))
         (div ((class . "row"))
           (div ((class . "col l6 s12"))
             (h5 ((class . "white-text")) "Footer Content")
             (p ((class . "grey-text text-lighten-4"))
               "You can use rows and columns here to organize your footer content."))
           (div ((class . "col l4 offset-l2 s12"))
             (h5 ((class . "white-text")) "Links")
             (ul nil
               (li nil
                 (a ((class . "grey-text text-lighten-3") (href . "#!")) "Link 1"))
               (li nil (a ((class . "grey-text text-lighten-3") (href . "#!")) "Link 2"))
               (li nil (a ((class . "grey-text text-lighten-3") (href . "#!")) "Link 3"))
               (li nil
                 (a ((class . "grey-text text-lighten-3") (href . "#!")) "Link 4"))))))
       (div ((class . "footer-copyright"))
         (div ((class . "container")) "© 2014 Copyright Text"
              (a ((class . "grey-text text-lighten-4 right") (href . "#!")) "More Links"))))))
