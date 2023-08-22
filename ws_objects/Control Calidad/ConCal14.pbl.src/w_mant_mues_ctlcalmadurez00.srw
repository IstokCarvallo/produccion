$PBExportHeader$w_mant_mues_ctlcalmadurez00.srw
forward
global type w_mant_mues_ctlcalmadurez00 from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_ctlcalmadurez00
end type
type uo_selespecie from uo_seleccion_especie within w_mant_mues_ctlcalmadurez00
end type
end forward

global type w_mant_mues_ctlcalmadurez00 from w_mant_tabla
string tag = "Maestro Parametros Madurez"
integer width = 2789
string title = "Maestro Parametros Madurez"
st_1 st_1
uo_selespecie uo_selespecie
end type
global w_mant_mues_ctlcalmadurez00 w_mant_mues_ctlcalmadurez00

type variables
w_mant_deta_ctlcalmadurez iw_mantencion
end variables

event ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True
istr_mant.Argumento[1]	=	String(uo_SelEspecie.Codigo)

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 AND pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)
end event

event ue_imprimir;SetPointer(HourGlass!)

Integer	li_zona
Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO DE PARAMETROS DE MADUREZ"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_ctlcalmadurez"
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(uo_SelEspecie.Codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	vinf.dw_1.Sort()
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta
DO
	ll_fila	= dw_1.Retrieve(uo_SelEspecie.Codigo)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_mant_mues_ctlcalmadurez00.create
int iCurrent
call super::create
this.st_1=create st_1
this.uo_selespecie=create uo_selespecie
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.uo_selespecie
end on

on w_mant_mues_ctlcalmadurez00.destroy
call super::destroy
destroy(this.st_1)
destroy(this.uo_selespecie)
end on

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

istr_mant.borra	= True
istr_mant.agrega	= False
istr_mant.Argumento[1]	=	String(uo_SelEspecie.Codigo)

OpenWithParm(iw_mantencion, istr_mant)  
														
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

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelEspecie.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelEspecie.Seleccion(False,False)
	uo_SelEspecie.Codigo									= Integer(Message.StringParm)
	uo_SelEspecie.dw_Seleccion.Object.Codigo[1] = Integer(Message.StringParm)
	uo_SelEspecie.Bloquear(True)
	
	buscar			= "Codigo:Nmadu_codigo,Descripcion:Smadu_nombre"
	ordenar			= "Codigo:madu_codigo,Descripcion:madu_nombre"
End If
end event

event resize;call super::resize;//dw_1.Resize(This.WorkSpaceWidth() - 510,This.WorkSpaceHeight() - dw_1.y - 75)
//
//dw_1.x					= 78
//dw_1.y					= 377
//gb_1.x 					= This.WorkSpaceWidth() - 351
//gb_1.y 					= 40
//gb_1.width				= 275
//gb_1.height				= 997
//pb_insertar.x			= This.WorkSpaceWidth() - 292
//pb_insertar.y			= 125
//pb_insertar.width		= 156
//pb_insertar.height	= 133
//pb_eliminar.x			= This.WorkSpaceWidth() - 292
//pb_eliminar.y			= 308
//pb_eliminar.width		= 156
//pb_eliminar.height	= 133
//pb_grabar.x				= This.WorkSpaceWidth() - 292
//pb_grabar.y				= 485
//pb_grabar.width		= 156
//pb_grabar.height		= 133
//pb_imprimir.x			= This.WorkSpaceWidth() - 292
//pb_imprimir.y			= 665
//pb_imprimir.width		= 156
//pb_imprimir.height	= 133
//pb_salir.x				= This.WorkSpaceWidth() - 292
//pb_salir.y				= 845
//pb_salir.width			= 156
//pb_salir.height		= 133
end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False
	istr_mant.Argumento[1]	=	String(uo_SelEspecie.Codigo)

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_ctlcalmadurez00
integer y = 356
integer width = 2258
integer height = 1464
integer taborder = 40
string dataobject = "dw_mues_ctlcalmadurez"
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_ctlcalmadurez00
integer y = 72
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_ctlcalmadurez00
integer x = 2455
integer y = 116
integer taborder = 20
end type

event pb_lectura::clicked;If IsNull(uo_SelEspecie.Codigo) Or uo_SelEspecie.Codigo = -1 Then
	Messagebox('Alerta','Debe ingresar una especie.', StopSign!, Ok!)
	Return
Else
	uo_SelEspecie.Bloquear(True)
	Parent.PostEvent("ue_recuperadatos")
End If
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_ctlcalmadurez00
integer x = 2455
integer taborder = 30
end type

event pb_nuevo::clicked;call super::clicked;pb_insertar.Enabled	= False
pb_grabar.Enabled		= False
pb_eliminar.Enabled	= False
pb_imprimir.Enabled	= False

end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_ctlcalmadurez00
integer x = 2455
integer y = 592
integer taborder = 50
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_ctlcalmadurez00
integer x = 2455
integer y = 768
integer taborder = 60
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_ctlcalmadurez00
integer x = 2455
integer y = 944
integer height = 152
integer taborder = 70
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_ctlcalmadurez00
integer x = 2455
integer y = 1140
integer taborder = 90
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_ctlcalmadurez00
integer x = 2455
integer y = 1512
end type

type st_1 from statictext within w_mant_mues_ctlcalmadurez00
integer x = 581
integer y = 176
integer width = 238
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_mant_mues_ctlcalmadurez00
event destroy ( )
integer x = 873
integer y = 168
integer height = 80
integer taborder = 30
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

