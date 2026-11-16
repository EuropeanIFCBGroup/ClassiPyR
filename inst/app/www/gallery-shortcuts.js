// Keyboard shortcuts for the image gallery.
//
// Every shortcut presses an existing toolbar button, so the server sees the
// same input as a mouse click:
//
//   Left / Right     previous / next page
//   Ctrl/Cmd + A     select the page (press again: all pages)
//   Esc              deselect; inside a text field, leave the field
//   Enter            relabel the selection to the class in "Relabel to"
//   /                put the cursor in "Relabel to"
//
// Keys are left alone while a dialog is open, and while the cursor is in a
// text field, dropdown or other form control (apart from Esc, and Enter in
// "Relabel to" once a class has been picked).

(function(root) {
  'use strict';

  var BUTTON_FOR_ACTION = {
    prev_page: 'prev_page',
    next_page: 'next_page',
    select_all: 'select_all',
    deselect: 'deselect_all',
    relabel: 'relabel_quick'
  };

  // Decide what a key press means. Pure: everything it needs to know about
  // the page is passed in `state`, so the rules can be read (and checked)
  // without a browser.
  //
  // key:   {key, ctrl, meta, alt, shift, repeat}
  // state: {modalOpen, inField, inButton, inRelabelBox, relabelBoxOpen,
  //         relabelBoxHasValue}
  // Returns an action name, or null to leave the key to the browser.
  function shortcutAction(key, state) {
    if (state.modalOpen || key.alt) return null;
    var modifier = key.ctrl || key.meta;

    if (state.inField) {
      if (key.key === 'Escape') return 'leave_field';
      var pickedClass = state.inRelabelBox && !state.relabelBoxOpen &&
        state.relabelBoxHasValue;
      if (key.key === 'Enter' && !modifier && !key.repeat && pickedClass) {
        return 'relabel';
      }
      return null;
    }

    if (modifier) {
      return (key.key === 'a' || key.key === 'A') && !key.shift ? 'select_all' : null;
    }

    switch (key.key) {
      case 'ArrowLeft': return key.shift ? null : 'prev_page';
      case 'ArrowRight': return key.shift ? null : 'next_page';
      case 'Escape': return 'deselect';
      // Enter on a focused button or link presses that button, as usual
      case 'Enter': return state.inButton || key.repeat ? null : 'relabel';
      case '/': return 'focus_relabel';
      default: return null;
    }
  }

  if (typeof module !== 'undefined' && module.exports) {
    module.exports = { shortcutAction: shortcutAction };
    return;
  }

  var $ = root.jQuery;
  var FIELD_SELECTOR = 'input, textarea, select, [contenteditable="true"], ' +
    '.selectize-control, .selectize-dropdown';

  function relabelBox() {
    var el = document.getElementById('new_class_quick');
    return el && el.selectize ? el.selectize : null;
  }

  function pageState(target) {
    var box = relabelBox();
    var inRelabelBox = !!box && $(target).closest(box.$wrapper).length > 0;
    return {
      modalOpen: $('.modal.show, .modal.in').length > 0,
      inField: $(target).closest(FIELD_SELECTOR).length > 0,
      inButton: $(target).closest('button, a, [role="button"]').length > 0,
      inRelabelBox: inRelabelBox,
      relabelBoxOpen: !!box && box.isOpen,
      relabelBoxHasValue: !!box && box.getValue() !== ''
    };
  }

  // Press a toolbar button, unless it is hidden (another tab is shown) or
  // disabled (a sample is loading or saving)
  function press(buttonId) {
    var button = $('#' + buttonId);
    var usable = button.length > 0 && button.is(':visible') &&
      !button.prop('disabled') && !button.hasClass('disabled');
    if (usable) button.trigger('click');
    return usable;
  }

  function run(action, target) {
    if (action === 'leave_field') {
      var box = relabelBox();
      if (box && $(target).closest(box.$wrapper).length) box.blur();
      target.blur();
      // Not handled: Esc also closes the dropdown it was pressed in
      return false;
    }
    if (action === 'focus_relabel') {
      var relabel = relabelBox();
      if (!relabel || !relabel.$wrapper.is(':visible')) return false;
      relabel.focus();
      return true;
    }
    if (action === 'relabel') {
      var pressed = press(BUTTON_FOR_ACTION.relabel);
      var picker = relabelBox();
      if (pressed && picker) picker.blur();
      return pressed;
    }
    return press(BUTTON_FOR_ACTION[action]);
  }

  // Capture phase: the state of the "Relabel to" dropdown is read before
  // selectize reacts to the same key press
  document.addEventListener('keydown', function(e) {
    var action = shortcutAction(
      { key: e.key, ctrl: e.ctrlKey, meta: e.metaKey, alt: e.altKey,
        shift: e.shiftKey, repeat: e.repeat },
      pageState(e.target)
    );
    if (action && run(action, e.target)) {
      e.preventDefault();
      e.stopPropagation();
    }
  }, true);

  // A toolbar button clicked with the mouse does not keep the focus, so
  // Enter relabels afterwards instead of pressing that button again.
  // Buttons reached with Tab keep it.
  $(document).on('mouseup', '.toolbar-btn', function() {
    this.blur();
  });
})(this);
