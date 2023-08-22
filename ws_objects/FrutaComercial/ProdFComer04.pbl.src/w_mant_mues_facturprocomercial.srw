$PBExportHeader$w_mant_mues_facturprocomercial.srw
forward
global type w_mant_mues_facturprocomercial from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_facturprocomercial
end type
type st_2 from statictext within w_mant_mues_facturprocomercial
end type
type st_3 from statictext within w_mant_mues_facturprocomercial
end type
type uo_muestraespecies from uo_seleccion_especie within w_mant_mues_facturprocomercial
end type
type uo_muestracliente from uo_seleccion_clientesprod within w_mant_mues_facturprocomercial
end type
type uo_muestraproductor from uo_seleccion_productor within w_mant_mues_facturprocomercial
end type
end forward

global type w_mant_mues_facturprocomercial from w_mant_tabla
integer width = 3525
integer height = 1904
string title = "MANTENCION GESTION VENTA"
st_1 st_1
st_2 st_2
st_3 st_3
uo_muestraespecies uo_muestraespecies
uo_muestracliente uo_muestracliente
uo_muestraproductor uo_muestraproductor
end type
global w_mant_mues_facturprocomercial w_mant_mues_facturprocomercial

type variables
w_mant_deta_facturprocomercial	 iw_mantencion

DataWindowChild	idwc_planta, idwc_variedades

end variables

event ue_imprimir;SetPointer(HourGlass!)

Long		fila  
Integer  li_pregunta

istr_info.titulo	= "INFORME VENTA DIRECTA FRUTA COMERCIAL"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

li_pregunta = MessageBox("Atención", "Desea un Informe General", Question!, YesNo!, 1)

IF li_pregunta = 1 THEN
   vinf.dw_1.DataObject = "dw_info_facturprocomercial_gral"
	vinf.dw_1.SetTransObject(sqlca)
	fila = vinf.dw_1.Retrieve(uo_MuestraProductor.Codigo)
ELSE
	vinf.dw_1.DataObject = "dw_info_facturprocomercial_encab"
	vinf.dw_1.SetTransObject(sqlca)
	li_pregunta = MessageBox("Atención", "¿Desea agregar información detallada al informe?", Question!, YesNo!, 1)
	fila = vinf.dw_1.Retrieve(uo_MuestraProductor.Codigo, uo_MuestraEspecies.Codigo, li_pregunta)
END IF

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
					StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

on w_mant_mues_facturprocomercial.create
int iCurrent
call super::create
this.st_1=create st_1
this.st_2=create st_2
this.st_3=create st_3
this.uo_muestraespecies=create uo_muestraespecies
this.uo_muestracliente=create uo_muestracliente
this.uo_muestraproductor=create uo_muestraproductor
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_3
this.Control[iCurrent+4]=this.uo_muestraespecies
this.Control[iCurrent+5]=this.uo_muestracliente
this.Control[iCurrent+6]=this.uo_muestraproductor
end on

on w_mant_mues_facturprocomercial.destroy
call super::destroy
destroy(this.st_1)
destroy(this.st_2)
destroy(this.st_3)
destroy(this.uo_muestraespecies)
destroy(this.uo_muestracliente)
destroy(this.uo_muestraproductor)
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

IF IsNull(uo_MuestraEspecies.Codigo) THEN lb_Cerrar	=	True
IF IsNull(uo_MuestraCliente.Codigo)  THEN lb_Cerrar	=	True
IF IsNull(uo_MuestraProductor.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_MuestraEspecies.Seleccion(False, False)
	uo_MuestraCliente.Seleccion(False, False)
	uo_MuestraProductor.Seleccion(False, False)
	uo_MuestraProductor.Filtra(-1)
	uo_MuestraEspecies.Inicia(Gi_CodEspecie)
	uo_MuestraCliente.Inicia(Gi_CodExport)
	
	/*planta*/
	dw_1.GetChild("plde_codigo", idwc_planta)
	idwc_planta.SetTransObject(sqlca)
	IF idwc_planta.Retrieve() = 0 THEN idwc_planta.InsertRow(0)
	
	/*variedades*/
	dw_1.GetChild("vari_codigo", idwc_variedades)
	idwc_variedades.SetTransObject(sqlca)
	IF idwc_variedades.Retrieve(-1) = 0 THEN idwc_variedades.InsertRow(0)
END IF
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
		
	ll_fila	= dw_1.Retrieve(uo_MuestraProductor.Codigo, uo_MuestraEspecies.Codigo, uo_MuestraCliente.Codigo)
	IF ll_fila = -1 THEN
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
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
	
/*planta*/
dw_1.GetChild("plde_codigo", idwc_planta)
idwc_planta.SetTransObject(sqlca)
IF idwc_planta.Retrieve() = 0 THEN
	idwc_planta.InsertRow(0)
END IF

/*variedades*/
dw_1.GetChild("vari_codigo", idwc_variedades)
idwc_variedades.SetTransObject(sqlca)
IF idwc_variedades.Retrieve(uo_MuestraEspecies.Codigo) = 0 THEN
	idwc_variedades.InsertRow(0)
END IF

LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_Mant.Agrega	= False
	istr_Mant.Borra		= False
	istr_Mant.Argumento[1]	=	String(uo_MuestraProductor.Codigo)
	istr_Mant.Argumento[2]	=	String(uo_MuestraEspecies.Codigo)
	istr_Mant.Argumento[3]	=	String(uo_MuestraCliente.Codigo)
	
	OpenWithParm(iw_mantencion, istr_mant)
END IF

end event

event ue_nuevo;call super::ue_nuevo;istr_Mant.Borra		= False
istr_Mant.Agrega	= True
istr_Mant.Argumento[1]	=	String(uo_MuestraProductor.Codigo)
istr_Mant.Argumento[2]	=	String(uo_MuestraEspecies.Codigo)
istr_Mant.Argumento[3]	=	String(uo_MuestraCliente.Codigo)
	
OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 And Not pb_eliminar.Enabled THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled	= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)

end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_facturprocomercial
integer x = 87
integer y = 388
integer width = 2642
integer height = 1356
integer taborder = 0
string dataobject = "dw_mues_facturprocomercial"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_facturprocomercial
integer x = 91
integer y = 16
integer width = 2638
integer height = 296
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_facturprocomercial
integer x = 2981
integer y = 116
integer taborder = 40
end type

event pb_lectura::clicked;call super::clicked;uo_MuestraEspecies.Bloquear(True)
uo_MuestraCliente.Bloquear(True)
uo_MuestraProductor.Bloquear(True)
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_facturprocomercial
integer x = 2990
integer taborder = 80
end type

event pb_nuevo::clicked;call super::clicked;uo_MuestraEspecies.Bloquear(False)
uo_MuestraCliente.Bloquear(False)
uo_MuestraProductor.Bloquear(False)
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_facturprocomercial
integer x = 2990
integer y = 584
integer taborder = 50
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_facturprocomercial
integer x = 2990
integer y = 760
integer taborder = 90
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_facturprocomercial
integer x = 2990
integer y = 964
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_facturprocomercial
integer x = 2990
integer y = 1128
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_facturprocomercial
integer x = 2990
integer y = 1516
integer taborder = 100
end type

type st_1 from statictext within w_mant_mues_facturprocomercial
integer x = 1381
integer y = 168
integer width = 297
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Productor"
boolean focusrectangle = false
end type

type st_2 from statictext within w_mant_mues_facturprocomercial
integer x = 1381
integer y = 68
integer width = 247
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Especie"
boolean focusrectangle = false
end type

type st_3 from statictext within w_mant_mues_facturprocomercial
integer x = 146
integer y = 168
integer width = 251
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Cliente"
boolean focusrectangle = false
end type

type uo_muestraespecies from uo_seleccion_especie within w_mant_mues_facturprocomercial
integer x = 1701
integer y = 60
integer height = 84
integer taborder = 30
boolean bringtotop = true
end type

on uo_muestraespecies.destroy
call uo_seleccion_especie::destroy
end on

type uo_muestracliente from uo_seleccion_clientesprod within w_mant_mues_facturprocomercial
integer x = 466
integer y = 164
integer height = 84
integer taborder = 30
boolean bringtotop = true
end type

on uo_muestracliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_muestraproductor from uo_seleccion_productor within w_mant_mues_facturprocomercial
integer x = 1710
integer y = 164
integer height = 84
integer taborder = 30
boolean bringtotop = true
end type

on uo_muestraproductor.destroy
call uo_seleccion_productor::destroy
end on

