{application, ps_barcode,
 [{description, "barcode image generator based on ps-barcode"},
  {vsn, "1.0"},
  {modules, [ps_barcode_app, ps_barcode_supervisor, barcode_data, wand, ps_bc]},
  {registered, [ps_bc, wand, ps_barcode_supervisor]},
  {applications, [kernel, stdlib]},
  {mod, {ps_barcode_app, []}},
  {start_phases, []}]}.