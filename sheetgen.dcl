sheetgen_page1 : dialog {
  label = "SheetGen - Layout Grid Setup";

  : text {
    label = "Check that your drawing properties are entered and your title block is set.";
  }
  : text {
    label = "Part numbers longer than 4 characters may need the DIESEL expression adjusted.";
  }

  spacer;

  : boxed_column {
    label = "Mode";

    : radio_column {
      key = "modegrp";
      : radio_button {
        key = "mode_new";
        label = "Create a new series of sheets";
      }
      : radio_button {
        key = "mode_add";
        label = "Add more sheets after the tabs already in this drawing";
      }
    }
  }

  : boxed_column {
    label = "Source Layout";

    : text {
      label = "To add sheets: copy your last tab, then run this in Add mode with that tab as the source.";
    }

    : row {
      : column {
        : text { label = "Copy sheets from layout:"; }
        : popup_list { key = "srclayout"; width = 30; }
      }
      : column {
        : text { label = "Grid position it shows now:"; }
        : edit_box { key = "srcpos"; width = 6; edit_limit = 6; }
      }
      : column {
        : text { label = "Grid position of first new sheet:"; }
        : edit_box { key = "firstpos"; width = 6; edit_limit = 6; }
      }
    }

    : toggle {
      key = "reuse";
      label = "Reuse the source layout as the first sheet of this batch";
    }
  }

  : boxed_column {
    label = "Layout Grid Configuration";

    : row {
      : column {
        : text { label = "Columns:"; }
        : edit_box { key = "cols"; width = 5; edit_limit = 6; }
      }
      : column {
        : text { label = "Rows:"; }
        : edit_box { key = "rows"; width = 5; edit_limit = 6; }
      }
      : column {
        : text { label = "Layouts in Last Row:"; }
        : edit_box { key = "lastrow"; width = 5; edit_limit = 6; }
      }
    }

    : row {
      : column {
        : text { label = "Horizontal Spacing:"; }
        : edit_box { key = "hspace"; width = 10; edit_limit = 20; }
      }
      : column {
        : text { label = "Vertical Spacing:"; }
        : edit_box { key = "vspace"; width = 10; edit_limit = 20; }
      }
    }
  }

  : boxed_column {
    label = "Part Number and SD Information";

    : text {
      label = "Separate values with spaces. Enter a single value to run a sequence.";
    }

    : column {
      : text { label = "Part Numbers (first part number ONLY for sequential):"; }
      : edit_box { key = "partnums"; width = 80; edit_limit = 255; }
    }

    : column {
      : text { label = "Quantities (single value applies to every sheet):"; }
      : edit_box { key = "quantities"; width = 80; edit_limit = 255; }
    }

    : column {
      : text { label = "SD Numbers (first SD number ONLY for sequential):"; }
      : edit_box { key = "sdnums"; width = 80; edit_limit = 255; }
    }
  }

  spacer;

  : errtile { width = 70; }

  : row {
    // NOTE: do not key this "accept". That is a reserved DCL key whose
    // built in action closes the dialog before the action expression runs.
    : button { key = "next"; label = "Next >"; is_default = true; }
    : button { key = "cancel"; label = "Cancel"; is_cancel = true; }
  }
}
