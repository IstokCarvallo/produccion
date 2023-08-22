$PBExportHeader$w_info_despacho_nave_var_env_ser.srw
forward
global type w_info_despacho_nave_var_env_ser from w_info_despacho_nave_por_productor
end type
end forward

global type w_info_despacho_nave_var_env_ser from w_info_despacho_nave_por_productor
integer width = 3374
integer height = 1500
string title = "DESPACHO DE NAVE VAR. /ENV. / SER."
end type
global w_info_despacho_nave_var_env_ser w_info_despacho_nave_var_env_ser

on w_info_despacho_nave_var_env_ser.create
call super::create
end on

on w_info_despacho_nave_var_env_ser.destroy
call super::destroy
end on

type st_titulo from w_info_despacho_nave_por_productor`st_titulo within w_info_despacho_nave_var_env_ser
string text = "Resumen Nave por Variedad / Envase"
end type

type pb_acepta from w_info_despacho_nave_por_productor`pb_acepta within w_info_despacho_nave_var_env_ser
integer y = 256
end type

type pb_salir from w_info_despacho_nave_por_productor`pb_salir within w_info_despacho_nave_var_env_ser
integer y = 528
integer taborder = 0
end type

type dw_etiqueta from w_info_despacho_nave_por_productor`dw_etiqueta within w_info_despacho_nave_var_env_ser
end type

type dw_status from w_info_despacho_nave_por_productor`dw_status within w_info_despacho_nave_var_env_ser
end type

type sle_vari_hasta from w_info_despacho_nave_por_productor`sle_vari_hasta within w_info_despacho_nave_var_env_ser
integer y = 828
end type

type sle_vari_desde from w_info_despacho_nave_por_productor`sle_vari_desde within w_info_despacho_nave_var_env_ser
integer y = 708
end type

type cb_1 from w_info_despacho_nave_por_productor`cb_1 within w_info_despacho_nave_var_env_ser
integer y = 832
end type

type cb_busc_var_d from w_info_despacho_nave_por_productor`cb_busc_var_d within w_info_despacho_nave_var_env_ser
integer y = 712
end type

type em_vari_hasta from w_info_despacho_nave_por_productor`em_vari_hasta within w_info_despacho_nave_var_env_ser
integer y = 828
end type

type em_vari_desde from w_info_despacho_nave_por_productor`em_vari_desde within w_info_despacho_nave_var_env_ser
integer y = 708
end type

type st_5 from w_info_despacho_nave_por_productor`st_5 within w_info_despacho_nave_var_env_ser
integer x = 142
integer y = 876
end type

type st_4 from w_info_despacho_nave_por_productor`st_4 within w_info_despacho_nave_var_env_ser
end type

type st_3 from w_info_despacho_nave_por_productor`st_3 within w_info_despacho_nave_var_env_ser
integer x = 142
integer y = 1168
end type

type dw_especie from w_info_despacho_nave_por_productor`dw_especie within w_info_despacho_nave_var_env_ser
integer x = 457
integer y = 864
end type

type st_2 from w_info_despacho_nave_por_productor`st_2 within w_info_despacho_nave_var_env_ser
integer y = 336
end type

type st_1 from w_info_despacho_nave_por_productor`st_1 within w_info_despacho_nave_var_env_ser
integer y = 236
end type

type dw_prod_hasta from w_info_despacho_nave_por_productor`dw_prod_hasta within w_info_despacho_nave_var_env_ser
integer y = 324
end type

type dw_prod_desde from w_info_despacho_nave_por_productor`dw_prod_desde within w_info_despacho_nave_var_env_ser
integer x = 448
integer y = 600
end type

type gb_5 from w_info_despacho_nave_por_productor`gb_5 within w_info_despacho_nave_var_env_ser
integer x = 78
integer y = 796
end type

type gb_4 from w_info_despacho_nave_por_productor`gb_4 within w_info_despacho_nave_var_env_ser
integer x = 78
integer y = 416
integer height = 360
end type

type gb_7 from w_info_despacho_nave_por_productor`gb_7 within w_info_despacho_nave_var_env_ser
end type

type gb_8 from w_info_despacho_nave_por_productor`gb_8 within w_info_despacho_nave_var_env_ser
end type

type gb_6 from w_info_despacho_nave_por_productor`gb_6 within w_info_despacho_nave_var_env_ser
end type

type cbx_todas from w_info_despacho_nave_por_productor`cbx_todas within w_info_despacho_nave_var_env_ser
integer taborder = 0
end type

type dw_planta from w_info_despacho_nave_por_productor`dw_planta within w_info_despacho_nave_var_env_ser
integer taborder = 0
end type

type gb_3 from w_info_despacho_nave_por_productor`gb_3 within w_info_despacho_nave_var_env_ser
integer y = 168
end type

type cbx_calibre from w_info_despacho_nave_por_productor`cbx_calibre within w_info_despacho_nave_var_env_ser
integer taborder = 0
end type

type cbx_serie from w_info_despacho_nave_por_productor`cbx_serie within w_info_despacho_nave_var_env_ser
integer taborder = 0
end type

type cbx_etiqueta from w_info_despacho_nave_por_productor`cbx_etiqueta within w_info_despacho_nave_var_env_ser
integer taborder = 0
end type

type cbx_status from w_info_despacho_nave_por_productor`cbx_status within w_info_despacho_nave_var_env_ser
integer taborder = 0
end type

type cbx_categoria from w_info_despacho_nave_por_productor`cbx_categoria within w_info_despacho_nave_var_env_ser
integer taborder = 0
end type

type cbx_envase from w_info_despacho_nave_por_productor`cbx_envase within w_info_despacho_nave_var_env_ser
integer taborder = 0
end type

type cbx_variedad from w_info_despacho_nave_por_productor`cbx_variedad within w_info_despacho_nave_var_env_ser
integer taborder = 0
end type

type cbx_productor from w_info_despacho_nave_por_productor`cbx_productor within w_info_despacho_nave_var_env_ser
integer taborder = 0
end type

type cbx_prod from w_info_despacho_nave_por_productor`cbx_prod within w_info_despacho_nave_var_env_ser
integer x = 448
integer y = 484
end type

type dw_vari from w_info_despacho_nave_por_productor`dw_vari within w_info_despacho_nave_var_env_ser
integer x = 457
integer y = 1164
integer height = 88
end type

type cbx_vari from w_info_despacho_nave_por_productor`cbx_vari within w_info_despacho_nave_var_env_ser
integer x = 462
integer y = 1044
end type

type dw_1 from w_info_despacho_nave_por_productor`dw_1 within w_info_despacho_nave_var_env_ser
integer x = 283
integer y = 256
end type

type gb_9 from w_info_despacho_nave_por_productor`gb_9 within w_info_despacho_nave_var_env_ser
integer x = 78
integer y = 172
end type

type rb_1 from w_info_despacho_nave_por_productor`rb_1 within w_info_despacho_nave_var_env_ser
integer y = 268
end type

type rb_2 from w_info_despacho_nave_por_productor`rb_2 within w_info_despacho_nave_var_env_ser
integer y = 268
end type

type dw_embarques from w_info_despacho_nave_por_productor`dw_embarques within w_info_despacho_nave_var_env_ser
integer x = 1733
integer y = 532
end type

type st_6 from w_info_despacho_nave_por_productor`st_6 within w_info_despacho_nave_var_env_ser
integer x = 1733
integer y = 660
end type

type st_7 from w_info_despacho_nave_por_productor`st_7 within w_info_despacho_nave_var_env_ser
integer x = 2341
integer y = 660
end type

type em_nro_embar from w_info_despacho_nave_por_productor`em_nro_embar within w_info_despacho_nave_var_env_ser
integer x = 1733
integer y = 744
end type

type em_fech_embar from w_info_despacho_nave_por_productor`em_fech_embar within w_info_despacho_nave_var_env_ser
integer x = 2345
integer y = 744
end type

type gb_emb from w_info_despacho_nave_por_productor`gb_emb within w_info_despacho_nave_var_env_ser
integer x = 1701
integer y = 448
end type

