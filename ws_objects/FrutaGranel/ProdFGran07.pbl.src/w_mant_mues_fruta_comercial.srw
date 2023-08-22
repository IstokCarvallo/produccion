$PBExportHeader$w_mant_mues_fruta_comercial.srw
forward
global type w_mant_mues_fruta_comercial from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_fruta_comercial
end type
type st_3 from statictext within w_mant_mues_fruta_comercial
end type
type dw_planta from datawindow within w_mant_mues_fruta_comercial
end type
type ddlb_1 from dropdownlistbox within w_mant_mues_fruta_comercial
end type
type st_2 from statictext within w_mant_mues_fruta_comercial
end type
type st_4 from statictext within w_mant_mues_fruta_comercial
end type
type ddlb_2 from dropdownlistbox within w_mant_mues_fruta_comercial
end type
type dw_2 from datawindow within w_mant_mues_fruta_comercial
end type
type st_5 from statictext within w_mant_mues_fruta_comercial
end type
type st_6 from statictext within w_mant_mues_fruta_comercial
end type
type cb_1 from commandbutton within w_mant_mues_fruta_comercial
end type
type dw_orden from datawindow within w_mant_mues_fruta_comercial
end type
end forward

global type w_mant_mues_fruta_comercial from w_mant_tabla
integer width = 3698
integer height = 1996
st_1 st_1
st_3 st_3
dw_planta dw_planta
ddlb_1 ddlb_1
st_2 st_2
st_4 st_4
ddlb_2 ddlb_2
dw_2 dw_2
st_5 st_5
st_6 st_6
cb_1 cb_1
dw_orden dw_orden
end type
global w_mant_mues_fruta_comercial w_mant_mues_fruta_comercial

type variables
DataWindowChild idwc_planta
DataWindowChild idwc_orden

uo_plantadesp 	 iuo_plantadesp

integer	ii_filaorden
end variables

on w_mant_mues_fruta_comercial.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_3=create st_3
this.dw_planta=create dw_planta
this.ddlb_1=create ddlb_1
this.st_2=create st_2
this.st_4=create st_4
this.ddlb_2=create ddlb_2
this.dw_2=create dw_2
this.st_5=create st_5
this.st_6=create st_6
this.cb_1=create cb_1
this.dw_orden=create dw_orden
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.dw_planta
this.Control[iCurrent+4]=this.ddlb_1
this.Control[iCurrent+5]=this.st_2
this.Control[iCurrent+6]=this.st_4
this.Control[iCurrent+7]=this.ddlb_2
this.Control[iCurrent+8]=this.dw_2
this.Control[iCurrent+9]=this.st_5
this.Control[iCurrent+10]=this.st_6
this.Control[iCurrent+11]=this.cb_1
this.Control[iCurrent+12]=this.dw_orden
end on

on w_mant_mues_fruta_comercial.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_3)
destroy(this.dw_planta)
destroy(this.ddlb_1)
destroy(this.st_2)
destroy(this.st_4)
destroy(this.ddlb_2)
destroy(this.dw_2)
destroy(this.st_5)
destroy(this.st_6)
destroy(this.cb_1)
destroy(this.dw_orden)
end on

event open;call super::open;/*
istr_mant.argumento[1]	=	Código de la Planta
istr_mant.argumento[2]	=	Número de Orden
istr_mant.argumento[3]	=	Códigp de la Especie
istr_mant.argumento[4]	=	Código del Productor
istr_mant.argumento[5]	=	Código de la Variedad
istr_mant.argumento[6]	=	Tipo de Proceso
istr_mant.argumento[7]	=	Seguimiento(orpr_niveld)

*/
pb_nuevo.PostEvent(Clicked!)

iuo_plantadesp	  = Create uo_plantadesp	

dw_planta.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(SqlCa)
IF idwc_planta.Retrieve(gi_codexport) = 0 THEN
	MessageBox("Atención","Falta Registrar Plantas")
ELSE
	dw_planta.SetTransObject(SqlCa)
	dw_planta.InsertRow(0)
END IF

dw_orden.GetChild("orpr_numero", idwc_orden)
idwc_orden.SetTransObject(SqlCa)
IF idwc_orden.Retrieve(integer(gstr_ParamPlanta.CodigoPlanta)) = 0 THEN
	MessageBox("Atención","Falta Registrar Ordenes")
ELSE
	dw_orden.SetTransObject(SqlCa)
	dw_orden.InsertRow(0)
	dw_2.settransobject(sqlca)
	dw_2.retrieve(integer(gstr_ParamPlanta.CodigoPlanta))
END IF


ddlb_1.SelectItem(1)

buscar	= "Código planta:Nplde_codigo,Tipo Orden:Norpr_tipord"
ordenar	= "Código planta:plde_codigo,Tipo Orden:orpr_tipord"

istr_mant.argumento[1]	=	String(gstr_ParamPlanta.CodigoPlanta)
istr_mant.argumento[6]	=	'4'

//dw_planta.Object.plde_codigo.BackGround.Color = RGB(192,192,192)
//dw_planta.Enabled = FALSE
end event

event ue_nuevo();call super::ue_nuevo;dw_Planta.SetItem(1,"plde_codigo",gstr_ParamPlanta.CodigoPlanta)

end event

event ue_borrar();call super::ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra		= True
istr_mant.agrega	= False

//OpenWithParm(iw_mantencion, istr_mant)  // La Ventana iw_mantencion debe ser una Variable de Instancia del Tipo de

istr_mant = Message.PowerObjectParm

IF istr_mant.respuesta = 1 THEN
	IF dw_1.DeleteRow(0) = 1 THEN
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	ELSE
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	END IF

 IF dw_1.RowCount() = 0 THEN
		pb_eliminar.Enabled = False
	ELSE
		il_fila = dw_1.GetRow()
		dw_1.SelectRow(il_fila,True)
	END IF
END IF

istr_mant.borra	 = False
end event

event ue_imprimir();SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "TITULO DEL INFORME"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve()

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
END IF

SetPointer(Arrow!)
end event

event ue_modifica();call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

//	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_recuperadatos();call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	dw_1.SetRedraw(True)
//	ll_fila	= dw_1.Retrieve(Integer(istr_Mant.Argumento[1]),Integer(Istr_Mant.Argumento[2]),+ &
//									Date(Mid(istr_mant.Argumento[3], 1, 10)),Date(Mid(istr_mant.Argumento[4], 1, 10)))

	ll_fila	= dw_1.Retrieve(Integer(istr_Mant.Argumento[1]),Integer(Istr_Mant.Argumento[2]))
								
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SetFocus()
	ELSE	
		MessageBox(	"Atención", "No Existe Información.", &
										Information!)
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN
	Close(This)
END IF
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_fruta_comercial
integer x = 55
integer y = 508
integer width = 3163
integer height = 864
string dataobject = "dw_mant_mues_fruta_comercial"
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_fruta_comercial
integer x = 46
integer y = 40
integer width = 3163
integer height = 412
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_fruta_comercial
integer x = 3355
integer y = 104
integer taborder = 40
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_fruta_comercial
integer x = 3351
integer y = 400
integer taborder = 50
end type

event pb_nuevo::clicked;call super::clicked;//dw_1.Reset()
//dw_2.Reset()

Parent.TriggerEvent("ue_nuevo")
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_fruta_comercial
integer x = 3351
integer y = 580
integer taborder = 60
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_fruta_comercial
integer x = 3351
integer y = 760
integer taborder = 70
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_fruta_comercial
integer x = 3351
integer y = 940
integer taborder = 80
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_fruta_comercial
integer x = 3351
integer y = 1120
integer taborder = 90
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_fruta_comercial
integer x = 3351
integer y = 1504
integer taborder = 100
end type

type st_1 from statictext within w_mant_mues_fruta_comercial
integer x = 114
integer y = 100
integer width = 210
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type st_3 from statictext within w_mant_mues_fruta_comercial
integer x = 1655
integer y = 100
integer width = 393
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
string text = "Tipo Proceso"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_mant_mues_fruta_comercial
string tag = "Planta"
integer x = 704
integer y = 100
integer width = 878
integer height = 88
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
end type

event itemchanged;//Integer li_null
//
//IF iuo_plantadesp.existe(Integer(data),True,SQLCa) = True THEN
//	istr_Mant.Argumento[1] = Data
//ELSE
//	dw_planta.SetItem(1,"plde_codigo",SetNull(li_null))
//	Return 1
//END IF	
end event

event itemerror;Return 1
end event

type ddlb_1 from dropdownlistbox within w_mant_mues_fruta_comercial
string tag = "Tipo proceso"
integer x = 2386
integer y = 100
integer width = 745
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
boolean sorted = false
string item[] = {"Orden Proceso","Re-Proceso","","","","","","","","","","",""}
borderstyle borderstyle = stylelowered!
end type

event selectionchanged;IF Index = 1 THEN	
	istr_mant.argumento[2] = '4'
ELSE
	istr_mant.argumento[2] = '5'
END IF	


IF istr_Mant.Argumento[2] <> "" AND istr_Mant.Argumento[3] <> "" AND &
   istr_Mant.Argumento[4] <> "" THEN
	pb_lectura.Enabled		=	True
END IF		



end event

type st_2 from statictext within w_mant_mues_fruta_comercial
integer x = 114
integer y = 220
integer width = 535
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Orden de Proceso"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_4 from statictext within w_mant_mues_fruta_comercial
integer x = 1655
integer y = 220
integer width = 622
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Tipo de Seguimiento"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type ddlb_2 from dropdownlistbox within w_mant_mues_fruta_comercial
integer x = 2386
integer y = 220
integer width = 745
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
string text = "none"
boolean sorted = false
string item[] = {"Productor","Huerto","Cuartel"}
borderstyle borderstyle = stylelowered!
end type

type dw_2 from datawindow within w_mant_mues_fruta_comercial
boolean visible = false
integer x = 695
integer y = 1488
integer width = 2139
integer height = 380
boolean bringtotop = true
boolean enabled = false
boolean titlebar = true
string title = "Informacion de la Orden de Proceso"
string dataobject = "dw_mues_spro_ordenproceso"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_5 from statictext within w_mant_mues_fruta_comercial
integer x = 114
integer y = 340
integer width = 270
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Predio"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_6 from statictext within w_mant_mues_fruta_comercial
integer x = 1655
integer y = 340
integer width = 535
integer height = 88
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Cuartel"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cb_1 from commandbutton within w_mant_mues_fruta_comercial
integer x = 1467
integer y = 220
integer width = 105
integer height = 92
integer taborder = 20
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "..."
end type

event clicked;Str_busqueda	lstr_busq

lstr_busq.argum[1] = istr_mant.argumento[1]
lstr_busq.argum[6] = istr_mant.argumento[6]

OpenWithParm(w_busc_orden_proceso, lstr_busq)

lstr_busq	= Message.PowerObjectParm

IF lstr_busq.argum[2] <> "" THEN
	istr_mant.argumento[2] = lstr_busq.argum[2]
	istr_mant.argumento[3] = lstr_busq.argum[3]
	istr_mant.argumento[4] = lstr_busq.argum[4]
	istr_mant.argumento[5] = lstr_busq.argum[5]
	istr_Mant.Argumento[6] = lstr_busq.argum[6]
	istr_Mant.Argumento[7] = lstr_busq.argum[7]	
	
	dw_orden.SetItem(1,"orpr_numero",Integer(istr_mant.Argumento[2]))
	ddlb_2.SelectItem(integer(istr_Mant.Argumento[7]))
END IF

RETURN
end event

type dw_orden from datawindow within w_mant_mues_fruta_comercial
integer x = 704
integer y = 220
integer width = 640
integer height = 88
integer taborder = 10
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_ordenproceso"
boolean border = false
boolean livescroll = true
end type

event itemchanged;
ii_filaorden	=	row

istr_mant.argumento[3]	=	String(dw_2.object.espe_codigo[ii_filaorden])
istr_mant.argumento[4]	=	String(dw_2.object.prod_codigo[ii_filaorden])
istr_mant.argumento[5]	=	String(dw_2.object.vari_codigo[ii_filaorden])
istr_mant.argumento[6]	=	String(dw_2.object.orpr_tippro[ii_filaorden])
end event

