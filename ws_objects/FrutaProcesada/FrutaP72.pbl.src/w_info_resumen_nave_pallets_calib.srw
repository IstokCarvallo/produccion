$PBExportHeader$w_info_resumen_nave_pallets_calib.srw
forward
global type w_info_resumen_nave_pallets_calib from w_info_despacho_nave_por_productor
end type
end forward

global type w_info_resumen_nave_pallets_calib from w_info_despacho_nave_por_productor
integer width = 3090
integer height = 1132
string title = "DESPACHO DE NAVES  PRODUCTOR / VARIEDAD"
end type
global w_info_resumen_nave_pallets_calib w_info_resumen_nave_pallets_calib

on w_info_resumen_nave_pallets_calib.create
call super::create
end on

on w_info_resumen_nave_pallets_calib.destroy
call super::destroy
end on

type st_computador from w_info_despacho_nave_por_productor`st_computador within w_info_resumen_nave_pallets_calib
end type

type st_usuario from w_info_despacho_nave_por_productor`st_usuario within w_info_resumen_nave_pallets_calib
end type

type st_temporada from w_info_despacho_nave_por_productor`st_temporada within w_info_resumen_nave_pallets_calib
end type

type p_logo from w_info_despacho_nave_por_productor`p_logo within w_info_resumen_nave_pallets_calib
end type

type st_titulo from w_info_despacho_nave_por_productor`st_titulo within w_info_resumen_nave_pallets_calib
string text = "Despacho Nave por Pallets / Calibre"
end type

type pb_acepta from w_info_despacho_nave_por_productor`pb_acepta within w_info_resumen_nave_pallets_calib
integer x = 1792
integer y = 752
integer taborder = 80
boolean enabled = true
end type

event pb_acepta::clicked;Integer	fila
String	l_s_titulo

l_s_titulo	= 'Saldos por Partida'
istr_info.titulo	= 'Movimiento de Fruta Procesada '+l_s_titulo 

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_compuesta_pallet_list"
vinf.dw_1.SetTransObject(sqlca)

//fila = vinf.dw_1.Retrieve(em_nro_embar.Text,dw_especie.GetItemNumber(1,'espe_codigo'),Integer(em_vari_desde.Text))
fila = vinf.dw_1.Retrieve()

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
				"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
				
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	StopSign!, Ok!)
	
ELSE
		F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF
SetPointer(Arrow!)		
end event

type pb_salir from w_info_despacho_nave_por_productor`pb_salir within w_info_resumen_nave_pallets_calib
integer x = 2085
integer y = 756
integer taborder = 90
end type

type dw_etiqueta from w_info_despacho_nave_por_productor`dw_etiqueta within w_info_resumen_nave_pallets_calib
end type

type dw_status from w_info_despacho_nave_por_productor`dw_status within w_info_resumen_nave_pallets_calib
end type

type sle_vari_hasta from w_info_despacho_nave_por_productor`sle_vari_hasta within w_info_resumen_nave_pallets_calib
integer x = 1993
integer y = 576
end type

type sle_vari_desde from w_info_despacho_nave_por_productor`sle_vari_desde within w_info_resumen_nave_pallets_calib
integer x = 1993
integer y = 448
end type

type cb_1 from w_info_despacho_nave_por_productor`cb_1 within w_info_resumen_nave_pallets_calib
integer x = 1897
integer y = 580
end type

type cb_busc_var_d from w_info_despacho_nave_por_productor`cb_busc_var_d within w_info_resumen_nave_pallets_calib
integer x = 1897
integer y = 452
end type

type em_vari_hasta from w_info_despacho_nave_por_productor`em_vari_hasta within w_info_resumen_nave_pallets_calib
integer x = 1669
integer y = 576
integer taborder = 0
end type

type em_vari_desde from w_info_despacho_nave_por_productor`em_vari_desde within w_info_resumen_nave_pallets_calib
integer x = 1669
integer y = 448
end type

type st_5 from w_info_despacho_nave_por_productor`st_5 within w_info_resumen_nave_pallets_calib
integer x = 146
integer y = 520
end type

type st_4 from w_info_despacho_nave_por_productor`st_4 within w_info_resumen_nave_pallets_calib
integer x = 1399
integer y = 460
end type

type st_3 from w_info_despacho_nave_por_productor`st_3 within w_info_resumen_nave_pallets_calib
integer x = 146
integer y = 820
boolean enabled = true
end type

type dw_especie from w_info_despacho_nave_por_productor`dw_especie within w_info_resumen_nave_pallets_calib
integer x = 475
integer y = 508
integer taborder = 20
end type

type st_2 from w_info_despacho_nave_por_productor`st_2 within w_info_resumen_nave_pallets_calib
integer x = 137
integer y = 340
end type

type st_1 from w_info_despacho_nave_por_productor`st_1 within w_info_resumen_nave_pallets_calib
integer x = 137
integer y = 240
end type

type dw_prod_hasta from w_info_despacho_nave_por_productor`dw_prod_hasta within w_info_resumen_nave_pallets_calib
integer x = 407
integer y = 328
boolean enabled = false
end type

type dw_prod_desde from w_info_despacho_nave_por_productor`dw_prod_desde within w_info_resumen_nave_pallets_calib
boolean visible = false
integer x = 407
integer y = 228
integer taborder = 0
end type

type gb_5 from w_info_despacho_nave_por_productor`gb_5 within w_info_resumen_nave_pallets_calib
integer x = 82
integer y = 432
end type

type gb_4 from w_info_despacho_nave_por_productor`gb_4 within w_info_resumen_nave_pallets_calib
boolean visible = false
integer height = 296
boolean enabled = false
end type

type gb_7 from w_info_despacho_nave_por_productor`gb_7 within w_info_resumen_nave_pallets_calib
end type

type gb_8 from w_info_despacho_nave_por_productor`gb_8 within w_info_resumen_nave_pallets_calib
end type

type gb_6 from w_info_despacho_nave_por_productor`gb_6 within w_info_resumen_nave_pallets_calib
end type

type cbx_todas from w_info_despacho_nave_por_productor`cbx_todas within w_info_resumen_nave_pallets_calib
integer taborder = 0
end type

type dw_planta from w_info_despacho_nave_por_productor`dw_planta within w_info_resumen_nave_pallets_calib
integer taborder = 0
end type

type gb_3 from w_info_despacho_nave_por_productor`gb_3 within w_info_resumen_nave_pallets_calib
boolean visible = false
boolean enabled = false
end type

type cbx_calibre from w_info_despacho_nave_por_productor`cbx_calibre within w_info_resumen_nave_pallets_calib
integer taborder = 0
end type

type cbx_serie from w_info_despacho_nave_por_productor`cbx_serie within w_info_resumen_nave_pallets_calib
integer taborder = 0
end type

type cbx_etiqueta from w_info_despacho_nave_por_productor`cbx_etiqueta within w_info_resumen_nave_pallets_calib
integer taborder = 0
end type

type cbx_status from w_info_despacho_nave_por_productor`cbx_status within w_info_resumen_nave_pallets_calib
integer taborder = 0
end type

type cbx_categoria from w_info_despacho_nave_por_productor`cbx_categoria within w_info_resumen_nave_pallets_calib
integer taborder = 0
end type

type cbx_envase from w_info_despacho_nave_por_productor`cbx_envase within w_info_resumen_nave_pallets_calib
integer x = 1390
integer y = 1080
integer taborder = 0
boolean enabled = false
end type

type cbx_variedad from w_info_despacho_nave_por_productor`cbx_variedad within w_info_resumen_nave_pallets_calib
boolean visible = true
integer x = 503
integer y = 680
integer taborder = 30
boolean checked = true
end type

type cbx_productor from w_info_despacho_nave_por_productor`cbx_productor within w_info_resumen_nave_pallets_calib
integer taborder = 0
end type

type cbx_prod from w_info_despacho_nave_por_productor`cbx_prod within w_info_resumen_nave_pallets_calib
boolean visible = false
integer x = 105
integer y = 480
integer taborder = 0
boolean enabled = false
end type

type dw_vari from w_info_despacho_nave_por_productor`dw_vari within w_info_resumen_nave_pallets_calib
integer x = 475
integer y = 816
integer taborder = 40
end type

type cbx_vari from w_info_despacho_nave_por_productor`cbx_vari within w_info_resumen_nave_pallets_calib
boolean visible = false
integer x = 507
integer y = 644
integer taborder = 0
boolean enabled = false
end type

type dw_1 from w_info_despacho_nave_por_productor`dw_1 within w_info_resumen_nave_pallets_calib
integer x = 128
integer y = 268
integer height = 96
end type

type gb_9 from w_info_despacho_nave_por_productor`gb_9 within w_info_resumen_nave_pallets_calib
integer x = 82
integer y = 188
integer width = 1294
integer height = 224
end type

type rb_1 from w_info_despacho_nave_por_productor`rb_1 within w_info_resumen_nave_pallets_calib
boolean visible = false
integer x = 192
integer y = 336
integer taborder = 0
boolean enabled = false
end type

type rb_2 from w_info_despacho_nave_por_productor`rb_2 within w_info_resumen_nave_pallets_calib
boolean visible = false
integer x = 773
integer y = 336
integer taborder = 0
boolean enabled = false
end type

type dw_embarques from w_info_despacho_nave_por_productor`dw_embarques within w_info_resumen_nave_pallets_calib
integer x = 1792
integer y = 272
integer taborder = 50
end type

type st_6 from w_info_despacho_nave_por_productor`st_6 within w_info_resumen_nave_pallets_calib
integer x = 1801
integer y = 400
end type

type st_7 from w_info_despacho_nave_por_productor`st_7 within w_info_resumen_nave_pallets_calib
integer x = 2377
integer y = 412
end type

type em_nro_embar from w_info_despacho_nave_por_productor`em_nro_embar within w_info_resumen_nave_pallets_calib
integer x = 1801
integer y = 480
integer taborder = 60
end type

type em_fech_embar from w_info_despacho_nave_por_productor`em_fech_embar within w_info_resumen_nave_pallets_calib
integer x = 2391
integer y = 484
integer taborder = 70
end type

type gb_emb from w_info_despacho_nave_por_productor`gb_emb within w_info_resumen_nave_pallets_calib
integer x = 1723
integer y = 188
integer taborder = 0
end type

type gb_10 from groupbox within w_info_resumen_nave_pallets_calib
int X=1335
int Y=756
int Width=699
int Height=380
string Text="Agrupa"
BorderStyle BorderStyle=StyleLowered!
long TextColor=33554432
long BackColor=67108864
int TextSize=-10
int Weight=700
string FaceName="Arial"
FontCharSet FontCharSet=Ansi!
FontFamily FontFamily=Swiss!
FontPitch FontPitch=Variable!
end type

