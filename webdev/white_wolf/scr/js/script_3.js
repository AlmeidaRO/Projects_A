$(document).ready(function () {
    $("#main-slider").owlCarousel({
        items: 2,
        itemsDesktop: [1000, 2],
        itemsDesktopSmall: [980, 1],
        itemsTablet: [768, 1],
        pagination: true,
        navigation: true,
        navigationText: ["<", ">"],
        autoPlay: true
    });
});