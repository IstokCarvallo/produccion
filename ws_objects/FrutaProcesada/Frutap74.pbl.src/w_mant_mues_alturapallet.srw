$PBExportHeader$w_mant_mues_alturapallet.srw
$PBExportComments$Mantenedor de Inspectores.
forward
global type w_mant_mues_alturapallet from w_mant_directo
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_alturapallet
end type
type uo_selplanta from uo_seleccion_plantas within w_mant_mues_alturapallet
end type
type st_1 from statictext within w_mant_mues_alturapallet
end type
type st_2 from statictext within w_mant_mues_alturapallet
end type
type st_3 from statictext within w_mant_mues_alturapallet
end type
type em_desde from editmask within w_mant_mues_alturapallet
end type
type em_hasta from editmask within w_mant_mues_alturapallet
end type
end forward

global type w_mant_mues_alturapallet from w_mant_directo
integer width = 2953
integer height = 1972
string title = "MANTENCIÓN ALTURA PALLET"
uo_selcliente uo_selcliente
uo_selplanta uo_selplanta
st_1 st_1
st_2 st_2
st_3 st_3
em_desde em_desde
em_hasta em_hasta
end type
global w_mant_mues_alturapallet w_mant_mues_alturapallet

type variables
DataWindowChild	idwc_Altura
end variables

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPLanta.Codigo, Long(em_Desde.Text), Long(em_Hasta.Text))

	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
		il_fila						= 1
		pb_grabar.Enabled	= True
		
		dw_1.GetChild('tpem_codigo', idwc_Altura)
		idwc_Altura.SetTransObject(Sqlca)
		idwc_Altura.Retrieve(uo_SelCliente.Codigo, dw_1.Object.emba_codigo[1])
	END IF

LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_mant_mues_alturapallet.create
int iCurrent
call super::create
this.uo_selcliente=create uo_selcliente
this.uo_selplanta=create uo_selplanta
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.em_desde=create em_desde
this.em_hasta=create em_hasta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_selcliente
this.Control[iCurrent+2]=this.uo_selplanta
this.Control[iCurrent+3]=this.st_1
this.Control[iCurrent+4]=this.st_2
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.em_desde
this.Control[iCurrent+7]=this.em_hasta
end on

on w_mant_mues_alturapallet.destroy
call super::destroy
destroy(this.uo_selcliente)
destroy(this.uo_selplanta)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.em_desde)
destroy(this.em_hasta)
end on

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "MANTENCIÓN ALTURA PALLET"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = ""
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelPLanta.Codigo, Long(em_Desde.Text), Long(em_Hasta.Text))

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Sort()
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

event ue_antesguardar;call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF dw_1.RowCount() > 0 THEN
	
//	IF Isnull(dw_1.Object.frre_codigo[il_fila]) OR dw_1.Object.frre_codigo[il_fila] = 0 THEN
//		li_cont ++
//		ls_mensaje 			= ls_mensaje + "~nCódigo de Fruta Recepción"
//		ls_colu[li_cont]	= "frre_codigo"
//	END IF
//	
//	IF Isnull(dw_1.Object.frre_descri[il_fila]) OR dw_1.Object.frre_descri[il_fila] = "" THEN
//		li_cont ++
//		ls_mensaje 			= ls_mensaje + "~nDescripción de Fruta Recepción"
//		ls_colu[li_cont]	= "frre_descri"
//	END IF
//	
//	IF Isnull(dw_1.Object.frre_abrevi[il_fila]) OR dw_1.Object.frre_abrevi[il_fila] = "" THEN
//		li_cont ++
//		ls_mensaje 			= ls_mensaje + "~nAbreviación de Fruta Recepción"
//		ls_colu[li_cont]	= "frre_abrevi"
//	END IF
//	
//	IF li_cont > 0 THEN
//		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
//		dw_1.SetColumn(ls_colu[1])
//		dw_1.SetFocus()
//		Message.DoubleParm = -1
//	END IF

ELSE
	pb_Grabar.Enabled		=	False
	pb_Eliminar.Enabled	=	False
	pb_Imprimir.Enabled	=	False
END IF
end event

event open;call super::open;Boolean	lb_Cerrar = False

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelPlanta.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	uo_SelPlanta.Filtra(1)
	uo_SelPlanta.Inicia(gi_CodPlanta)

	dw_1.GetChild('tpem_codigo', idwc_Altura)
	idwc_Altura.SetTransObject(Sqlca)
	idwc_Altura.Retrieve(uo_SelCliente.Codigo, 'Z')
		
End If
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_alturapallet
integer x = 73
integer y = 44
integer width = 2272
integer height = 364
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_alturapallet
boolean visible = false
integer x = 2505
integer y = 380
integer taborder = 60
boolean enabled = false
end type

event pb_nuevo::clicked;call super::clicked;pb_lectura.Enabled	= True
pb_insertar.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled		= False
pb_imprimir.Enabled	= False

il_fila					= 0

IF gstr_apl.CodigoSistema <> 23 THEN
	IF gi_codexport = gi_cliebase THEN
	//	IF gi_codplanta <> 2 AND gi_codplanta <> 3 AND gi_codplanta <> 4 THEN
			istr_mant.Solo_Consulta = True
	//	END IF	
	END IF	
END IF	


end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_alturapallet
integer x = 2505
integer y = 108
integer taborder = 50
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_alturapallet
boolean visible = false
integer x = 2505
integer y = 764
integer taborder = 90
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_alturapallet
boolean visible = false
integer x = 2505
integer y = 584
integer taborder = 80
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_alturapallet
integer x = 2505
integer y = 1508
integer taborder = 120
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_alturapallet
boolean visible = false
integer x = 2505
integer y = 1124
integer taborder = 110
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_alturapallet
integer x = 2505
integer y = 944
integer taborder = 100
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_alturapallet
integer x = 73
integer y = 456
integer width = 2272
integer height = 1292
integer taborder = 70
string dataobject = "dw_mant_mues_alturapalletfruta"
boolean hscrollbar = true
end type

event dw_1::itemchanged;call super::itemchanged;Integer		li_null

SetNull(li_null)

CHOOSE CASE dwo.Name
	CASE ""


END CHOOSE

end event

event dw_1::rowfocuschanged;call super::rowfocuschanged;//IF CurrentRow > 0 AND il_fila > 0 THEN
//	ias_campo[1] = String(dw_1.Object.cctc_codigo[il_fila])
//	ias_campo[2] = dw_1.Object.cctc_nombres[il_fila]
//	ias_campo[3] = dw_1.Object.cctc_abrevi[il_fila]
//END IF

Integer li_Fila

li_Fila = dw_1.RowCount()

IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila				=	GetRow()
	pb_grabar.Enabled	=	True
END IF

end event

event dw_1::itemfocuschanged;call super::itemfocuschanged;CHOOSE CASE dwo.Name

	CASE "copa_nombre"
			pb_grabar.Enabled	=	True

END CHOOSE

end event

type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_alturapallet
integer x = 544
integer y = 80
integer height = 96
integer taborder = 30
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selplanta from uo_seleccion_plantas within w_mant_mues_alturapallet
integer x = 544
integer y = 240
integer height = 100
integer taborder = 60
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type st_1 from statictext within w_mant_mues_alturapallet
integer x = 165
integer y = 96
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Cliente"
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_mues_alturapallet
integer x = 165
integer y = 256
integer width = 306
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Planta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_mant_mues_alturapallet
integer x = 1522
integer y = 104
integer width = 325
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
string text = "Folio Pallet"
boolean focusrectangle = false
end type

type em_desde from editmask within w_mant_mues_alturapallet
integer x = 1522
integer y = 180
integer width = 741
integer height = 84
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#######"
end type

type em_hasta from editmask within w_mant_mues_alturapallet
integer x = 1522
integer y = 284
integer width = 741
integer height = 84
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#######"
end type

