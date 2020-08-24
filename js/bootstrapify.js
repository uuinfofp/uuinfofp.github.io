$(document).ready(function(){

    $("table").addClass("table table-hover");

    // description lists
    $("dl").addClass("row");
    $("dt").addClass("col-sm-2");
    $("dd").addClass("col-sm-10");

    // data tag to badge
    // $("span").filter(".tag").addClass(function() { return  "badge badge-"+$(this).data("tagName")});
    $("span").filter(".tag").addClass("badge badge-primary");

});
