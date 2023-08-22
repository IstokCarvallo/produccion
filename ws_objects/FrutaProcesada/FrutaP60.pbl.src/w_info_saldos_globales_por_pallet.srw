$PBExportHeader$w_info_saldos_globales_por_pallet.srw
forward
global type w_info_saldos_globales_por_pallet from w_info_saldos_globales
end type
type em_nro_pallet from editmask within w_info_saldos_globales_por_pallet
end type
end forward

global type w_info_saldos_globales_por_pallet from w_info_saldos_globales
integer width = 2999
string title = "EXISTENCIAS GLOBALES POR PALLET"
em_nro_pallet em_nro_pallet
end type
global w_info_saldos_globales_por_pallet w_info_saldos_globales_por_pallet

on w_info_saldos_globales_por_pallet.create
int iCurrent
call super::create
this.em_nro_pallet=create em_nro_pallet
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.em_nro_pallet
end on

on w_info_saldos_globales_por_pallet.destroy
call super::destroy
destroy(this.em_nro_pallet)
end on

type st_computador from w_info_saldos_globales`st_computador within w_info_saldos_globales_por_pallet
end type

type st_usuario from w_info_saldos_globales`st_usuario within w_info_saldos_globales_por_pallet
end type

type st_temporada from w_info_saldos_globales`st_temporada within w_info_saldos_globales_por_pallet
end type

type p_logo from w_info_saldos_globales`p_logo within w_info_saldos_globales_por_pallet
end type

type st_titulo from w_info_saldos_globales`st_titulo within w_info_saldos_globales_por_pallet
string text = "Informe de Existencias por Pallet Fruta Procesada"
end type

type pb_acepta from w_info_saldos_globales`pb_acepta within w_info_saldos_globales_por_pallet
integer x = 2610
integer taborder = 190
end type

event pb_acepta::clicked;Integer	fila,variedad, productor,codigo_especie,codigo_status
String	l_s_titulo

l_s_titulo	= 'Saldos Globales por Pallet'
istr_info.titulo	= 'Movimiento de Fruta Procesada '+l_s_titulo 

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_despachos_globales_por_pallet"
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Long(em_nro_pallet.Text),codigo_status,variedad,productor,codigo_especie)

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

type pb_salir from w_info_saldos_globales`pb_salir within w_info_saldos_globales_por_pallet
integer x = 2629
end type

type dw_etiqueta from w_info_saldos_globales`dw_etiqueta within w_info_saldos_globales_por_pallet
integer taborder = 90
end type

type dw_status from w_info_saldos_globales`dw_status within w_info_saldos_globales_por_pallet
integer y = 1324
end type

type sle_vari_hasta from w_info_saldos_globales`sle_vari_hasta within w_info_saldos_globales_por_pallet
end type

type sle_vari_desde from w_info_saldos_globales`sle_vari_desde within w_info_saldos_globales_por_pallet
end type

type cb_1 from w_info_saldos_globales`cb_1 within w_info_saldos_globales_por_pallet
end type

type cb_busc_var_d from w_info_saldos_globales`cb_busc_var_d within w_info_saldos_globales_por_pallet
end type

type em_vari_hasta from w_info_saldos_globales`em_vari_hasta within w_info_saldos_globales_por_pallet
end type

type em_vari_desde from w_info_saldos_globales`em_vari_desde within w_info_saldos_globales_por_pallet
end type

type st_5 from w_info_saldos_globales`st_5 within w_info_saldos_globales_por_pallet
end type

type st_4 from w_info_saldos_globales`st_4 within w_info_saldos_globales_por_pallet
end type

type st_3 from w_info_saldos_globales`st_3 within w_info_saldos_globales_por_pallet
end type

type dw_especie from w_info_saldos_globales`dw_especie within w_info_saldos_globales_por_pallet
end type

type st_2 from w_info_saldos_globales`st_2 within w_info_saldos_globales_por_pallet
end type

type st_1 from w_info_saldos_globales`st_1 within w_info_saldos_globales_por_pallet
end type

type dw_prod_hasta from w_info_saldos_globales`dw_prod_hasta within w_info_saldos_globales_por_pallet
end type

type dw_prod_desde from w_info_saldos_globales`dw_prod_desde within w_info_saldos_globales_por_pallet
end type

type gb_5 from w_info_saldos_globales`gb_5 within w_info_saldos_globales_por_pallet
end type

type gb_4 from w_info_saldos_globales`gb_4 within w_info_saldos_globales_por_pallet
end type

type gb_7 from w_info_saldos_globales`gb_7 within w_info_saldos_globales_por_pallet
integer height = 232
end type

type gb_8 from w_info_saldos_globales`gb_8 within w_info_saldos_globales_por_pallet
end type

type gb_6 from w_info_saldos_globales`gb_6 within w_info_saldos_globales_por_pallet
integer width = 823
string text = "N° Pallet"
end type

type cbx_todas from w_info_saldos_globales`cbx_todas within w_info_saldos_globales_por_pallet
boolean visible = false
end type

type dw_planta from w_info_saldos_globales`dw_planta within w_info_saldos_globales_por_pallet
boolean visible = false
integer taborder = 100
end type

type gb_3 from w_info_saldos_globales`gb_3 within w_info_saldos_globales_por_pallet
end type

type cbx_calibre from w_info_saldos_globales`cbx_calibre within w_info_saldos_globales_por_pallet
integer taborder = 210
end type

type cbx_serie from w_info_saldos_globales`cbx_serie within w_info_saldos_globales_por_pallet
integer taborder = 180
end type

type cbx_etiqueta from w_info_saldos_globales`cbx_etiqueta within w_info_saldos_globales_por_pallet
integer taborder = 170
end type

type cbx_status from w_info_saldos_globales`cbx_status within w_info_saldos_globales_por_pallet
integer taborder = 160
end type

type cbx_categoria from w_info_saldos_globales`cbx_categoria within w_info_saldos_globales_por_pallet
integer taborder = 150
end type

type cbx_envase from w_info_saldos_globales`cbx_envase within w_info_saldos_globales_por_pallet
integer taborder = 130
end type

type cbx_variedad from w_info_saldos_globales`cbx_variedad within w_info_saldos_globales_por_pallet
integer taborder = 120
end type

type cbx_productor from w_info_saldos_globales`cbx_productor within w_info_saldos_globales_por_pallet
integer taborder = 140
end type

type cbx_prod from w_info_saldos_globales`cbx_prod within w_info_saldos_globales_por_pallet
end type

type dw_vari from w_info_saldos_globales`dw_vari within w_info_saldos_globales_por_pallet
end type

type cbx_vari from w_info_saldos_globales`cbx_vari within w_info_saldos_globales_por_pallet
end type

type dw_1 from w_info_saldos_globales`dw_1 within w_info_saldos_globales_por_pallet
integer x = 338
end type

type gb_9 from w_info_saldos_globales`gb_9 within w_info_saldos_globales_por_pallet
end type

type em_nro_pallet from editmask within w_info_saldos_globales_por_pallet
integer x = 1792
integer y = 272
integer width = 622
integer height = 84
integer taborder = 110
boolean bringtotop = true
integer textsize = -9
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
borderstyle borderstyle = stylelowered!
string mask = "########"
end type

