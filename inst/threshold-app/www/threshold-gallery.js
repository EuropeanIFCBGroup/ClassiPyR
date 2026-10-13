// Gallery interactions of the ClassiPyR threshold review app.
//
// Everything a reviewer marks stays in the browser until "Confirm page" is
// pressed. The state of a card is kept as classes on the card itself:
//   loaded  - its image has loaded
//   seen    - the card has been on screen
//   failed  - its image could not be loaded
//   wrong   - the reviewer marked it as not being the class
// A new page replaces the cards, so nothing has to be reset by hand.
// Adapted from gallery_js() of the main app (inst/app/ui.R).
(function() {
  'use strict';

  var GALLERY = '#thr-gallery';
  var isDragging = false;
  var wasDragging = false;
  var startX = 0;
  var startY = 0;
  var selectionBox = null;

  // Only a card whose image has loaded can be marked
  function canMark(card) {
    return card.hasClass('loaded') && !card.hasClass('failed') &&
      !card.hasClass('unavailable');
  }

  // An unmarked image is recorded as correct, so it only counts once the
  // reviewer has had it on screen. Having loaded is not enough: browsers fetch
  // lazy images well below the visible part of the page.
  function isViewed(card) {
    return canMark(card) && card.hasClass('seen');
  }

  function updateCount() {
    var cards = $(GALLERY + ' .thr-card');
    var text = '';
    if (cards.length > 0) {
      var viewed = cards.filter(function() { return isViewed($(this)); }).length;
      var missing = cards.filter('.failed, .unavailable').length;
      text = cards.filter('.wrong').length + ' marked wrong; ' + viewed +
        ' of ' + cards.length + ' images viewed';
      if (missing > 0) text += ', ' + missing + ' unavailable';
      if (viewed + missing < cards.length) {
        text += ' (scroll down to view the rest; images you have not ' +
          'scrolled to are not recorded)';
      }
    }
    $('#thr-mark-count').text(text);
  }

  // A card counts as seen once at least half of it, or a part at least half
  // as tall as the window, has been on screen. The second rule is for images
  // taller than the window, which never show half of themselves at once; the
  // fine steps are what makes the observer report them at all.
  var SEEN_STEPS = [];
  for (var step = 0; step <= 20; step++) SEEN_STEPS.push(step / 20);

  var seenObserver = null;
  if ('IntersectionObserver' in window) {
    seenObserver = new IntersectionObserver(function(entries) {
      entries.forEach(function(entry) {
        if (!entry.isIntersecting) return;
        var tallEnough =
          entry.intersectionRect.height >= window.innerHeight / 2;
        if (entry.intersectionRatio >= 0.5 || tallEnough) {
          $(entry.target).addClass('seen');
          seenObserver.unobserve(entry.target);
        }
      });
      updateCount();
    }, {threshold: SEEN_STEPS});
  }

  function watchCards() {
    $(GALLERY + ' .thr-card').each(function() {
      var card = $(this);
      if (card.hasClass('seen') || card.hasClass('unavailable')) return;
      // Without an observer there is no way to tell, so fall back to treating
      // a loaded image as seen
      if (seenObserver) seenObserver.observe(this); else card.addClass('seen');
    });
  }

  function setImageState(img, state) {
    var card = $(img).closest('.thr-card');
    if (!card.length) return;
    if (state === 'failed') {
      // The placeholder takes the room the image had, so the cards after it
      // do not move under the pointer when an image fails late
      var width = img.getAttribute('width');
      var height = img.getAttribute('height');
      if (width && height) {
        card.find('.thr-placeholder').css({
          width: width + 'px', height: height + 'px', lineHeight: height + 'px'
        });
      }
      card.removeClass('loaded wrong').addClass('failed');
    } else if (!card.hasClass('failed')) {
      card.addClass('loaded');
    }
    updateCount();
  }

  // load and error events of images do not bubble: listen while capturing
  document.addEventListener('load', function(e) {
    if (e.target && e.target.tagName === 'IMG') setImageState(e.target, 'loaded');
  }, true);
  document.addEventListener('error', function(e) {
    if (e.target && e.target.tagName === 'IMG') setImageState(e.target, 'failed');
  }, true);

  // Images that finished loading before their event could be seen. A failure
  // is only ever taken from an error event: an image that has not started
  // loading must stay unreported, so that it comes back on a later page.
  function sweepLoaded() {
    $(GALLERY + ' .thr-card img').each(function() {
      if (this.complete && this.naturalWidth > 0) setImageState(this, 'loaded');
    });
    updateCount();
  }

  $(document).on('shiny:value', function(e) {
    if (e.name !== 'thr_gallery') return;
    setTimeout(function() {
      watchCards();
      sweepLoaded();
    }, 0);
  });

  // Single click toggles a mark
  $(document).on('click', '.thr-card', function() {
    if (wasDragging) {
      wasDragging = false;
      return;
    }
    var card = $(this);
    if (!canMark(card)) return;
    // A card that can be clicked is on screen
    card.addClass('seen').toggleClass('wrong');
    updateCount();
  });

  // Drag-select: start from anywhere in the gallery area that is not a card
  // (or with Shift held on a card)
  $(document).on('mousedown', '.thr-drag-area', function(e) {
    wasDragging = false;
    if (e.button !== 0) return;
    if ($(e.target).closest('.thr-card').length && !e.shiftKey) return;

    isDragging = true;
    // The selection box is position:fixed (viewport coordinates), so use
    // clientX/Y; pageX/Y would offset the box by the scroll distance
    startX = e.clientX;
    startY = e.clientY;

    selectionBox = $('#thr-selection-box');
    selectionBox.css({
      left: startX + 'px',
      top: startY + 'px',
      width: '0px',
      height: '0px',
      display: 'block'
    });

    e.preventDefault();
  });

  $(document).on('mousemove', function(e) {
    if (!isDragging) return;

    var width = Math.abs(e.clientX - startX);
    var height = Math.abs(e.clientY - startY);

    selectionBox.css({
      left: Math.min(startX, e.clientX) + 'px',
      top: Math.min(startY, e.clientY) + 'px',
      width: width + 'px',
      height: height + 'px'
    });

    if (width > 5 || height > 5) wasDragging = true;
  });

  $(document).on('mouseup', function() {
    if (!isDragging) return;
    isDragging = false;

    var boxRect = selectionBox[0].getBoundingClientRect();
    selectionBox.css('display', 'none');

    if (boxRect.width < 5 && boxRect.height < 5) {
      wasDragging = false;
      return;
    }

    $(GALLERY + ' .thr-card').each(function() {
      var cardRect = this.getBoundingClientRect();
      if (cardRect.left < boxRect.right && cardRect.right > boxRect.left &&
          cardRect.top < boxRect.bottom && cardRect.bottom > boxRect.top &&
          canMark($(this))) {
        $(this).addClass('seen wrong');
      }
    });
    updateCount();
    // wasDragging stays set until the click that follows this mouseup has
    // been swallowed, so a drag ending on a card does not toggle it back
    setTimeout(function() { wasDragging = false; }, 0);
  });

  // Confirm: report which images were viewed (loaded and on screen), which
  // failed and which are marked. "wrong" only ever holds viewed images. An
  // image that loaded but was never scrolled to is not reported, so it stays
  // pending and comes back on a later page.
  $(document).on('click', '#thr_confirm_btn', function() {
    var gallery = $(GALLERY);
    if (!gallery.length || !gallery.attr('data-token')) return;
    sweepLoaded();

    var loaded = [];
    var failed = [];
    var wrong = [];
    gallery.find('.thr-card').each(function() {
      var card = $(this);
      var id = parseInt(card.attr('data-draw'), 10);
      if (isNaN(id)) return;
      if (card.hasClass('failed')) {
        failed.push(id);
      } else if (isViewed(card)) {
        loaded.push(id);
        if (card.hasClass('wrong')) wrong.push(id);
      }
    });

    Shiny.setInputValue('thr_confirm', {
      token: gallery.attr('data-token'),
      loaded: loaded,
      failed: failed,
      wrong: wrong,
      nonce: Date.now() + Math.random()
    }, {priority: 'event'});

    // One confirmation per page: a second click while the server is still
    // working would only come back as "no longer the current page"
    var button = $(this).prop('disabled', true);
    $(document).one('shiny:idle', function() {
      button.prop('disabled', $(GALLERY + ' .thr-card').length === 0);
    });
  });

  $(function() {
    Shiny.addCustomMessageHandler('thrClearMarks', function(message) {
      $(GALLERY + ' .thr-card.wrong').removeClass('wrong');
      updateCount();
    });
  });
})();
