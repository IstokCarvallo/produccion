$PBExportHeader$w_info_despacho_nave_prod_nave.srw
forward
global type w_info_despacho_nave_prod_nave from w_info_despacho_nave_por_productor
end type
end forward

global type w_info_despacho_nave_prod_nave from w_info_despacho_nave_por_productor
integer height = 1424
string title = "DESPACHO DE NAVES POR PRODUCTOR / NAVE"
end type
global w_info_despacho_nave_prod_nave w_info_despacho_nave_prod_nave

on w_info_despacho_nave_prod_nave.create
call super::create
end on

on w_info_despacho_nave_prod_nave.destroy
call super::destroy
end on

type st_computador from w_info_despacho_nave_por_productor`st_computador within w_info_despacho_nave_prod_nave
end type

type st_usuario from w_info_despacho_nave_por_productor`st_usuario within w_info_despacho_nave_prod_nave
end type

type st_temporada from w_info_despacho_nave_por_productor`st_temporada within w_info_despacho_nave_prod_nave
end type

type p_logo from w_info_despacho_nave_por_productor`p_logo within w_info_despacho_nave_prod_nave
end type

type st_titulo from w_info_despacho_nave_por_productor`st_titulo within w_info_despacho_nave_prod_nave
string text = "Despacho Nave por Productor / Nave"
end type

type pb_acepta from w_info_despacho_nave_por_productor`pb_acepta within w_info_despacho_nave_prod_nave
boolean enabled = true
end type

event pb_acepta::clicked;Integer	fila,li_variedad,li_cliente
String	l_s_titulo

l_s_titulo	= 'Saldos por Partida'
istr_info.titulo	= 'Movimiento de Fruta Procesada '+l_s_titulo 

IF cbx_vari.checked THEN
	li_variedad=0
ELSE
	li_variedad=dw_vari.getitemNumber(1,"vari_codigo")
END IF
li_cliente=dw_1.Getitemnumber(1,"clie_codigo")
OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_nave_productor"
vinf.dw_1.SetTransObject(sqlca)
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

type pb_salir from w_info_despacho_nave_por_productor`pb_salir within w_info_despacho_nave_prod_nave
integer taborder = 0
end type

type dw_etiqueta from w_info_despacho_nave_por_productor`dw_etiqueta within w_info_despacho_nave_prod_nave
end type

type dw_status from w_info_despacho_nave_por_productor`dw_status within w_info_despacho_nave_prod_nave
end type

type sle_vari_hasta from w_info_despacho_nave_por_productor`sle_vari_hasta within w_info_despacho_nave_prod_nave
integer y = 868
end type

type sle_vari_desde from w_info_despacho_nave_por_productor`sle_vari_desde within w_info_despacho_nave_prod_nave
integer y = 756
end type

type cb_1 from w_info_despacho_nave_por_productor`cb_1 within w_info_despacho_nave_prod_nave
integer y = 872
end type

type cb_busc_var_d from w_info_despacho_nave_por_productor`cb_busc_var_d within w_info_despacho_nave_prod_nave
integer y = 760
end type

type em_vari_hasta from w_info_despacho_nave_por_productor`em_vari_hasta within w_info_despacho_nave_prod_nave
integer y = 868
end type

type em_vari_desde from w_info_despacho_nave_por_productor`em_vari_desde within w_info_despacho_nave_prod_nave
integer y = 756
end type

type st_5 from w_info_despacho_nave_por_productor`st_5 within w_info_despacho_nave_prod_nave
integer x = 146
integer y = 556
end type

type st_4 from w_info_despacho_nave_por_productor`st_4 within w_info_despacho_nave_prod_nave
integer y = 768
end type

type st_3 from w_info_despacho_nave_por_productor`st_3 within w_info_despacho_nave_prod_nave
integer y = 776
boolean enabled = true
end type

type dw_especie from w_info_despacho_nave_por_productor`dw_especie within w_info_despacho_nave_prod_nave
integer y = 556
end type

type st_2 from w_info_despacho_nave_por_productor`st_2 within w_info_despacho_nave_prod_nave
end type

type st_1 from w_info_despacho_nave_por_productor`st_1 within w_info_despacho_nave_prod_nave
end type

type dw_prod_hasta from w_info_despacho_nave_por_productor`dw_prod_hasta within w_info_despacho_nave_prod_nave
end type

type dw_prod_desde from w_info_despacho_nave_por_productor`dw_prod_desde within w_info_despacho_nave_prod_nave
boolean visible = false
end type

type gb_5 from w_info_despacho_nave_por_productor`gb_5 within w_info_despacho_nave_prod_nave
integer y = 480
end type

type gb_4 from w_info_despacho_nave_por_productor`gb_4 within w_info_despacho_nave_prod_nave
boolean visible = false
end type

type gb_7 from w_info_despacho_nave_por_productor`gb_7 within w_info_despacho_nave_prod_nave
boolean enabled = false
end type

type gb_8 from w_info_despacho_nave_por_productor`gb_8 within w_info_despacho_nave_prod_nave
end type

type gb_6 from w_info_despacho_nave_por_productor`gb_6 within w_info_despacho_nave_prod_nave
end type

type cbx_todas from w_info_despacho_nave_por_productor`cbx_todas within w_info_despacho_nave_prod_nave
integer taborder = 0
end type

type dw_planta from w_info_despacho_nave_por_productor`dw_planta within w_info_despacho_nave_prod_nave
integer taborder = 0
end type

type gb_3 from w_info_despacho_nave_por_productor`gb_3 within w_info_despacho_nave_prod_nave
boolean visible = false
end type

type cbx_calibre from w_info_despacho_nave_por_productor`cbx_calibre within w_info_despacho_nave_prod_nave
integer taborder = 0
end type

type cbx_serie from w_info_despacho_nave_por_productor`cbx_serie within w_info_despacho_nave_prod_nave
integer taborder = 0
end type

type cbx_etiqueta from w_info_despacho_nave_por_productor`cbx_etiqueta within w_info_despacho_nave_prod_nave
integer taborder = 0
end type

type cbx_status from w_info_despacho_nave_por_productor`cbx_status within w_info_despacho_nave_prod_nave
integer taborder = 0
end type

type cbx_categoria from w_info_despacho_nave_por_productor`cbx_categoria within w_info_despacho_nave_prod_nave
integer taborder = 0
end type

type cbx_envase from w_info_despacho_nave_por_productor`cbx_envase within w_info_despacho_nave_prod_nave
integer taborder = 0
end type

type cbx_variedad from w_info_despacho_nave_por_productor`cbx_variedad within w_info_despacho_nave_prod_nave
integer taborder = 0
end type

type cbx_productor from w_info_despacho_nave_por_productor`cbx_productor within w_info_despacho_nave_prod_nave
integer taborder = 0
end type

type cbx_prod from w_info_despacho_nave_por_productor`cbx_prod within w_info_despacho_nave_prod_nave
boolean visible = false
end type

type dw_vari from w_info_despacho_nave_por_productor`dw_vari within w_info_despacho_nave_prod_nave
integer y = 768
end type

type cbx_vari from w_info_despacho_nave_por_productor`cbx_vari within w_info_despacho_nave_prod_nave
integer y = 684
end type

type dw_1 from w_info_despacho_nave_por_productor`dw_1 within w_info_despacho_nave_prod_nave
integer x = 288
integer width = 1221
end type

type gb_9 from w_info_despacho_nave_por_productor`gb_9 within w_info_despacho_nave_prod_nave
integer height = 276
end type

type rb_1 from w_info_despacho_nave_por_productor`rb_1 within w_info_despacho_nave_prod_nave
boolean visible = false
integer y = 156
end type

type rb_2 from w_info_despacho_nave_por_productor`rb_2 within w_info_despacho_nave_prod_nave
boolean visible = false
integer y = 156
end type

type dw_embarques from w_info_despacho_nave_por_productor`dw_embarques within w_info_despacho_nave_prod_nave
integer y = 232
end type

type st_6 from w_info_despacho_nave_por_productor`st_6 within w_info_despacho_nave_prod_nave
integer y = 360
end type

type st_7 from w_info_despacho_nave_por_productor`st_7 within w_info_despacho_nave_prod_nave
integer y = 360
end type

type em_nro_embar from w_info_despacho_nave_por_productor`em_nro_embar within w_info_despacho_nave_prod_nave
integer y = 444
end type

type em_fech_embar from w_info_despacho_nave_por_productor`em_fech_embar within w_info_despacho_nave_prod_nave
integer y = 444
end type

type gb_emb from w_info_despacho_nave_por_productor`gb_emb within w_info_despacho_nave_prod_nave
integer y = 156
integer height = 528
end type

