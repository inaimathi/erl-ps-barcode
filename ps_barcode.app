{application, ps_barcode,
 [{description, "A barcode image server based on the ps-barcode generator"},
  {vsn, "1.0"},
  {modules, [ps_barcode_app, ps_barcode_supervisor, wand, ps_bc, barcode_data]},
  {registered, [ps_bc, wand, ps_barcode_supervisor]},
  {applications, [kernel, stdlib]},
  {mod, {ps_barcode_app, []}},
  {start_phases, []}]}.