/*
    Strata by HTML5 UP
    html5up.net | @ajlkn
    Free for personal and commercial use under the CCA 3.0 license (html5up.net/license)
*/

(function($) {
    const settings = {
        parallax: true,
        parallaxFactor: 20,
    };

    // Breakpoints
    skel.breakpoints({
        xlarge: '(max-width: 1800px)',
        large: '(max-width: 1280px)',
        medium: '(max-width: 980px)',
        small: '(max-width: 736px)',
        xsmall: '(max-width: 480px)',
    });

    $(function() {
        const $window = $(window);
        const $body = $('body');
        const $header = $('#header');

        // Disable animations/transitions until the page has loaded.
        $body.addClass('is-loading');
        $window.on('load', function() {
            $body.removeClass('is-loading');
        });

        // Enable touch mode for mobile devices.
        if (skel.vars.mobile) {
            $body.addClass('is-touch');
            window.setTimeout(() => {
                $window.scrollTop($window.scrollTop() + 1);
            }, 0);
        }

        // Placeholder polyfill
        $('form').placeholder();

        // Parallax background
        if (!skel.vars.mobile && settings.parallax) {
            skel.on('change', function() {
                if (skel.breakpoint('medium').active) {
                    $window.off('scroll.strata_parallax');
                    $header.css('background-position', 'top left, center center');
                } else {
                    $header.css('background-position', 'left 0px');
                    $window.on('scroll.strata_parallax', function() {
                        $header.css('background-position', `left ${-1 * (parseInt($window.scrollTop()) / settings.parallaxFactor)}px`);
                    });
                }
            });
        }

        // Lightbox gallery
        $window.on('load', function() {
            $('#two').poptrox({
                caption: $a => $a.next('h3').text(),
                overlayColor: '#2c2c2c',
                overlayOpacity: 0.85,
                selector: '.work-item a.image',
                usePopupCaption: true,
                usePopupNav: true,
                windowMargin: skel.breakpoint('small').active ? 0 : 50,
            });
        });

        // Slideshow
        let currentSlide = 0;
        const $slides = $('.slideshow img');

        function showSlide(index) {
            $slides.hide().eq(index).show();
        }

        function nextSlide() {
            currentSlide = (currentSlide + 1) % $slides.length;
            showSlide(currentSlide);
        }

        // Initialize the slideshow
        setInterval(nextSlide, 5000);  //This controls the length between slides. 
        showSlide(currentSlide);

        // Lightbox
        function openLightbox(image) {
            const lightbox = document.getElementById('lightbox');
            const lightboxImage = document.getElementById('lightbox-image');
            const lightboxCaption = document.getElementById('lightbox-caption');

            lightbox.style.display = 'flex';
            lightboxImage.src = image.src;
            lightboxCaption.innerText = image.alt;
        }

        function closeLightbox() {
            const lightbox = document.getElementById('lightbox');
            lightbox.style.display = 'none';
        }

        // Add global handlers for lightbox (optional)
        window.openLightbox = openLightbox;
        window.closeLightbox = closeLightbox;
    });
})(jQuery);
