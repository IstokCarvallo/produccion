$PBExportHeader$w_info_despacho_nave_prod_vari.srw
forward
global type w_info_despacho_nave_prod_vari from w_info_despacho_nave_por_productor
end type
end forward

global type w_info_despacho_nave_prod_vari from w_info_despacho_nave_por_productor
integer height = 1540
string title = "DESPACHO DE NAVES  PRODUCTOR / VARIEDAD"
end type
global w_info_despacho_nave_prod_vari w_info_despacho_nave_prod_vari

on w_info_despacho_nave_prod_vari.create
call super::create
end on

on w_info_despacho_nave_prod_vari.destroy
call super::destroy
end on

type st_computador from w_info_despacho_nave_por_productor`st_computador within w_info_despacho_nave_prod_vari
end type

type st_usuario from w_info_despacho_nave_por_productor`st_usuario within w_info_despacho_nave_prod_vari
end type

type st_temporada from w_info_despacho_nave_por_productor`st_temporada within w_info_despacho_nave_prod_vari
end type

type p_logo from w_info_despacho_nave_por_productor`p_logo within w_info_despacho_nave_prod_vari
end type

type st_titulo from w_info_despacho_nave_por_productor`st_titulo within w_info_despacho_nave_prod_vari
string text = "Despacho Nave por Productor / Variedad"
end type

type pb_acepta from w_info_despacho_nave_por_productor`pb_acepta within w_info_despacho_nave_prod_vari
integer x = 3031
integer y = 292
boolean enabled = true
end type

event pb_acepta::clicked;Integer	fila,li_cliente,li_variedad
String	l_s_titulo

l_s_titulo	= 'Saldos por Partida'
istr_info.titulo	= 'Movimiento de Fruta Procesada '+l_s_titulo 

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_nave_prodvarnav"
vinf.dw_1.SetTransObject(sqlca)
li_cliente=dw_1.getitemnumber(1,"clie_codigo")
IF cbx_vari.checked THEN
	li_variedad=0
ELSE
	li_variedad=dw_vari.getitemnumber(1,"vari_codigo")
END IF


fila = vinf.dw_1.Retrieve(li_cliente,em_nro_embar.Text,dw_especie.GetItemNumber(1,'espe_codigo'),li_variedad)

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

type pb_salir from w_info_despacho_nave_por_productor`pb_salir within w_info_despacho_nave_prod_vari
integer x = 3026
integer y = 568
integer taborder = 0
end type

type dw_etiqueta from w_info_despacho_nave_por_productor`dw_etiqueta within w_info_despacho_nave_prod_vari
end type

type dw_status from w_info_despacho_nave_por_productor`dw_status within w_info_despacho_nave_prod_vari
end type

type sle_vari_hasta from w_info_despacho_nave_por_productor`sle_vari_hasta within w_info_despacho_nave_prod_vari
integer y = 852
end type

type sle_vari_desde from w_info_despacho_nave_por_productor`sle_vari_desde within w_info_despacho_nave_prod_vari
integer y = 724
end type

type cb_1 from w_info_despacho_nave_por_productor`cb_1 within w_info_despacho_nave_prod_vari
integer y = 856
end type

type cb_busc_var_d from w_info_despacho_nave_por_productor`cb_busc_var_d within w_info_despacho_nave_prod_vari
integer y = 728
end type

type em_vari_hasta from w_info_despacho_nave_por_productor`em_vari_hasta within w_info_despacho_nave_prod_vari
integer x = 407
integer y = 852
end type

type em_vari_desde from w_info_despacho_nave_por_productor`em_vari_desde within w_info_despacho_nave_prod_vari
integer x = 407
integer y = 724
end type

type st_5 from w_info_despacho_nave_por_productor`st_5 within w_info_despacho_nave_prod_vari
integer x = 151
integer y = 596
end type

type st_4 from w_info_despacho_nave_por_productor`st_4 within w_info_despacho_nave_prod_vari
integer y = 736
end type

type st_3 from w_info_despacho_nave_por_productor`st_3 within w_info_despacho_nave_prod_vari
integer x = 133
integer y = 764
boolean enabled = true
end type

type dw_especie from w_info_despacho_nave_por_productor`dw_especie within w_info_despacho_nave_prod_vari
integer x = 466
integer y = 580
end type

type st_2 from w_info_despacho_nave_por_productor`st_2 within w_info_despacho_nave_prod_vari
integer x = 137
integer y = 340
end type

type st_1 from w_info_despacho_nave_por_productor`st_1 within w_info_despacho_nave_prod_vari
integer x = 137
integer y = 240
end type

type dw_prod_hasta from w_info_despacho_nave_por_productor`dw_prod_hasta within w_info_despacho_nave_prod_vari
integer x = 407
integer y = 328
end type

type dw_prod_desde from w_info_despacho_nave_por_productor`dw_prod_desde within w_info_despacho_nave_prod_vari
boolean visible = false
integer y = 564
end type

type gb_5 from w_info_despacho_nave_por_productor`gb_5 within w_info_despacho_nave_prod_vari
integer y = 480
integer height = 628
end type

type gb_4 from w_info_despacho_nave_por_productor`gb_4 within w_info_despacho_nave_prod_vari
boolean visible = false
integer y = 420
integer height = 296
end type

type gb_7 from w_info_despacho_nave_por_productor`gb_7 within w_info_despacho_nave_prod_vari
end type

type gb_8 from w_info_despacho_nave_por_productor`gb_8 within w_info_despacho_nave_prod_vari
end type

type gb_6 from w_info_despacho_nave_por_productor`gb_6 within w_info_despacho_nave_prod_vari
end type

type cbx_todas from w_info_despacho_nave_por_productor`cbx_todas within w_info_despacho_nave_prod_vari
end type

type dw_planta from w_info_despacho_nave_por_productor`dw_planta within w_info_despacho_nave_prod_vari
integer taborder = 0
end type

type gb_3 from w_info_despacho_nave_por_productor`gb_3 within w_info_despacho_nave_prod_vari
integer y = 184
end type

type cbx_calibre from w_info_despacho_nave_por_productor`cbx_calibre within w_info_despacho_nave_prod_vari
integer taborder = 0
end type

type cbx_serie from w_info_despacho_nave_por_productor`cbx_serie within w_info_despacho_nave_prod_vari
integer taborder = 0
end type

type cbx_etiqueta from w_info_despacho_nave_por_productor`cbx_etiqueta within w_info_despacho_nave_prod_vari
integer taborder = 0
end type

type cbx_status from w_info_despacho_nave_por_productor`cbx_status within w_info_despacho_nave_prod_vari
integer taborder = 0
end type

type cbx_categoria from w_info_despacho_nave_por_productor`cbx_categoria within w_info_despacho_nave_prod_vari
integer taborder = 0
end type

type cbx_envase from w_info_despacho_nave_por_productor`cbx_envase within w_info_despacho_nave_prod_vari
integer taborder = 0
end type

type cbx_variedad from w_info_despacho_nave_por_productor`cbx_variedad within w_info_despacho_nave_prod_vari
integer taborder = 0
end type

type cbx_productor from w_info_despacho_nave_por_productor`cbx_productor within w_info_despacho_nave_prod_vari
integer taborder = 0
end type

type cbx_prod from w_info_despacho_nave_por_productor`cbx_prod within w_info_despacho_nave_prod_vari
boolean visible = false
integer y = 476
end type

type dw_vari from w_info_despacho_nave_por_productor`dw_vari within w_info_despacho_nave_prod_vari
integer x = 466
integer y = 948
end type

type cbx_vari from w_info_despacho_nave_por_productor`cbx_vari within w_info_despacho_nave_prod_vari
integer x = 466
integer y = 840
end type

type dw_1 from w_info_despacho_nave_por_productor`dw_1 within w_info_despacho_nave_prod_vari
integer y = 280
end type

type gb_9 from w_info_despacho_nave_por_productor`gb_9 within w_info_despacho_nave_prod_vari
integer y = 188
integer height = 276
end type

type rb_1 from w_info_despacho_nave_por_productor`rb_1 within w_info_despacho_nave_prod_vari
integer y = 288
end type

type rb_2 from w_info_despacho_nave_por_productor`rb_2 within w_info_despacho_nave_prod_vari
integer y = 288
end type

type dw_embarques from w_info_despacho_nave_por_productor`dw_embarques within w_info_despacho_nave_prod_vari
integer y = 556
end type

type st_6 from w_info_despacho_nave_por_productor`st_6 within w_info_despacho_nave_prod_vari
integer x = 1769
integer y = 684
end type

type st_7 from w_info_despacho_nave_por_productor`st_7 within w_info_despacho_nave_prod_vari
integer x = 2318
integer y = 684
end type

type em_nro_embar from w_info_despacho_nave_por_productor`em_nro_embar within w_info_despacho_nave_prod_vari
integer x = 1769
integer y = 768
end type

type em_fech_embar from w_info_despacho_nave_por_productor`em_fech_embar within w_info_despacho_nave_prod_vari
integer x = 2395
integer y = 768
integer width = 352
end type

type gb_emb from w_info_despacho_nave_por_productor`gb_emb within w_info_despacho_nave_prod_vari
integer y = 480
end type

