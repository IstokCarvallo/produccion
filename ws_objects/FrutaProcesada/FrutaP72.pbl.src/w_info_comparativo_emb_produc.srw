$PBExportHeader$w_info_comparativo_emb_produc.srw
forward
global type w_info_comparativo_emb_produc from w_info_despacho_nave_por_productor
end type
end forward

global type w_info_comparativo_emb_produc from w_info_despacho_nave_por_productor
integer width = 3337
integer height = 1456
string title = "COMPARATIVO EMBARQUES v/s PRODUCCION"
end type
global w_info_comparativo_emb_produc w_info_comparativo_emb_produc

on w_info_comparativo_emb_produc.create
call super::create
end on

on w_info_comparativo_emb_produc.destroy
call super::destroy
end on

type st_titulo from w_info_despacho_nave_por_productor`st_titulo within w_info_comparativo_emb_produc
string text = "Corporativo de Embarque v/s Producción"
end type

type pb_acepta from w_info_despacho_nave_por_productor`pb_acepta within w_info_comparativo_emb_produc
integer x = 3040
integer y = 248
integer taborder = 130
boolean enabled = true
end type

type pb_salir from w_info_despacho_nave_por_productor`pb_salir within w_info_comparativo_emb_produc
integer y = 576
integer taborder = 0
end type

type dw_etiqueta from w_info_despacho_nave_por_productor`dw_etiqueta within w_info_comparativo_emb_produc
end type

type dw_status from w_info_despacho_nave_por_productor`dw_status within w_info_comparativo_emb_produc
end type

type sle_vari_hasta from w_info_despacho_nave_por_productor`sle_vari_hasta within w_info_comparativo_emb_produc
end type

type sle_vari_desde from w_info_despacho_nave_por_productor`sle_vari_desde within w_info_comparativo_emb_produc
fontcharset fontcharset = ansi!
long backcolor = 79741120
end type

type cb_1 from w_info_despacho_nave_por_productor`cb_1 within w_info_comparativo_emb_produc
end type

type cb_busc_var_d from w_info_despacho_nave_por_productor`cb_busc_var_d within w_info_comparativo_emb_produc
end type

type em_vari_hasta from w_info_despacho_nave_por_productor`em_vari_hasta within w_info_comparativo_emb_produc
end type

type em_vari_desde from w_info_despacho_nave_por_productor`em_vari_desde within w_info_comparativo_emb_produc
end type

type st_5 from w_info_despacho_nave_por_productor`st_5 within w_info_comparativo_emb_produc
end type

type st_4 from w_info_despacho_nave_por_productor`st_4 within w_info_comparativo_emb_produc
end type

type st_3 from w_info_despacho_nave_por_productor`st_3 within w_info_comparativo_emb_produc
end type

type dw_especie from w_info_despacho_nave_por_productor`dw_especie within w_info_comparativo_emb_produc
end type

type st_2 from w_info_despacho_nave_por_productor`st_2 within w_info_comparativo_emb_produc
end type

type st_1 from w_info_despacho_nave_por_productor`st_1 within w_info_comparativo_emb_produc
end type

type dw_prod_hasta from w_info_despacho_nave_por_productor`dw_prod_hasta within w_info_comparativo_emb_produc
end type

type dw_prod_desde from w_info_despacho_nave_por_productor`dw_prod_desde within w_info_comparativo_emb_produc
end type

type gb_5 from w_info_despacho_nave_por_productor`gb_5 within w_info_comparativo_emb_produc
end type

type gb_4 from w_info_despacho_nave_por_productor`gb_4 within w_info_comparativo_emb_produc
end type

type gb_7 from w_info_despacho_nave_por_productor`gb_7 within w_info_comparativo_emb_produc
end type

type gb_8 from w_info_despacho_nave_por_productor`gb_8 within w_info_comparativo_emb_produc
end type

type gb_6 from w_info_despacho_nave_por_productor`gb_6 within w_info_comparativo_emb_produc
end type

type cbx_todas from w_info_despacho_nave_por_productor`cbx_todas within w_info_comparativo_emb_produc
integer taborder = 0
end type

type dw_planta from w_info_despacho_nave_por_productor`dw_planta within w_info_comparativo_emb_produc
integer taborder = 0
end type

type gb_3 from w_info_despacho_nave_por_productor`gb_3 within w_info_comparativo_emb_produc
integer y = 156
end type

type cbx_calibre from w_info_despacho_nave_por_productor`cbx_calibre within w_info_comparativo_emb_produc
integer taborder = 0
end type

type cbx_serie from w_info_despacho_nave_por_productor`cbx_serie within w_info_comparativo_emb_produc
integer taborder = 0
end type

type cbx_etiqueta from w_info_despacho_nave_por_productor`cbx_etiqueta within w_info_comparativo_emb_produc
integer taborder = 0
end type

type cbx_status from w_info_despacho_nave_por_productor`cbx_status within w_info_comparativo_emb_produc
integer taborder = 0
end type

type cbx_categoria from w_info_despacho_nave_por_productor`cbx_categoria within w_info_comparativo_emb_produc
integer taborder = 0
end type

type cbx_envase from w_info_despacho_nave_por_productor`cbx_envase within w_info_comparativo_emb_produc
integer taborder = 0
end type

type cbx_variedad from w_info_despacho_nave_por_productor`cbx_variedad within w_info_comparativo_emb_produc
integer taborder = 0
end type

type cbx_productor from w_info_despacho_nave_por_productor`cbx_productor within w_info_comparativo_emb_produc
integer taborder = 0
end type

type cbx_prod from w_info_despacho_nave_por_productor`cbx_prod within w_info_comparativo_emb_produc
end type

type dw_vari from w_info_despacho_nave_por_productor`dw_vari within w_info_comparativo_emb_produc
integer taborder = 70
end type

type cbx_vari from w_info_despacho_nave_por_productor`cbx_vari within w_info_comparativo_emb_produc
integer taborder = 60
end type

type dw_1 from w_info_despacho_nave_por_productor`dw_1 within w_info_comparativo_emb_produc
end type

type gb_9 from w_info_despacho_nave_por_productor`gb_9 within w_info_comparativo_emb_produc
end type

type rb_1 from w_info_despacho_nave_por_productor`rb_1 within w_info_comparativo_emb_produc
integer taborder = 80
end type

type rb_2 from w_info_despacho_nave_por_productor`rb_2 within w_info_comparativo_emb_produc
integer taborder = 90
end type

type dw_embarques from w_info_despacho_nave_por_productor`dw_embarques within w_info_comparativo_emb_produc
integer x = 1733
integer taborder = 100
end type

type st_6 from w_info_despacho_nave_por_productor`st_6 within w_info_comparativo_emb_produc
end type

type st_7 from w_info_despacho_nave_por_productor`st_7 within w_info_comparativo_emb_produc
end type

type em_nro_embar from w_info_despacho_nave_por_productor`em_nro_embar within w_info_comparativo_emb_produc
integer taborder = 110
end type

type em_fech_embar from w_info_despacho_nave_por_productor`em_fech_embar within w_info_comparativo_emb_produc
integer taborder = 120
end type

type gb_emb from w_info_despacho_nave_por_productor`gb_emb within w_info_comparativo_emb_produc
integer taborder = 0
end type

