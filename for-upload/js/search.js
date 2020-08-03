
$.expr[":"].contains = $.expr.createPseudo(function (arg) {
    return function (elem) {
        return $(elem).text().toUpperCase().indexOf(arg.toUpperCase()) >= 0;
    };
});


var main = function () {
    $('#title-search-box').val("");
    $('input[type="checkbox"]').prop('checked', false);
    $('[data-toggle="popover"]').popover();
    $('#title-search-box').keyup(function () {
        var conts = $(this).val();
        if (conts === "")
            {
                $('.paperbubble').removeClass('searchbarhide');
            }
            else
            {
                $('.paperbubble').addClass('searchbarhide');
                $('.title-link:contains('+conts+')').parents('.paperbubble').removeClass('searchbarhide');
            }
    });
    $('.searchcheck').click(function () {
        var showSolo = $('input[name="check-solo"]').prop('checked');
        var showCERvR = $('input[name="check-cervr"]').prop('checked');
        var showOther = $('input[name="check-other"]').prop('checked');
        if (!(showSolo || showCERvR || showOther))
            {
                $('.paperbubble').removeClass('searchauthorhide');
            }
            else
            {
                $('.paperbubble').addClass('searchauthorhide');
                if (showSolo)
                {
                    $('.paperbubble.solo').removeClass('searchauthorhide');
                }
                if (showCERvR)
                {
                    $('.paperbubble.cervr').removeClass('searchauthorhide');
                }
                if (showOther)
                {
                    $('.paperbubble.other').removeClass('searchauthorhide');
                }
            }
    });
};

$(document).ready(main);