$PBExportHeader$w_mant_mues_romanas.srw
forward
global type w_mant_mues_romanas from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_romanas
end type
type dw_planta from datawindow within w_mant_mues_romanas
end type
end forward

global type w_mant_mues_romanas from w_mant_tabla
integer width = 4969
integer height = 2248
string title = "EQUIPOS CON ROMANA"
st_1 st_1
dw_planta dw_planta
end type
global w_mant_mues_romanas w_mant_mues_romanas

type variables
w_mant_deta_romanas iw_mantencion

datawindowchild idwc_Planta

uo_plantadesp	iuo_plantadesp
end variables

forward prototypes
public function boolean noexisteespecie (integer ai_especie)
end prototypes

public function boolean noexisteespecie (integer ai_especie);String ls_especie

SELECT	espe_nombre
	INTO	:ls_especie
	FROM	dbo.especies
	WHERE	espe_codigo = 	:ai_especie;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de Tabla Especies")
	
	RETURN TRUE
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Especie (" + String(ai_especie) + &
					"), no ha sido creado en tabla respectiva.~r~r" + &
					"Ingrese o seleccione otro Código.")
	
	RETURN TRUE
END IF

RETURN FALSE
end function

event ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)

end event

event ue_imprimir;//SetPointer(HourGlass!)
//
//Long		ll_fila 
//
//
//str_info	lstr_info
//
//lstr_info.titulo	= "MAESTRO DE DAÑOS Y DEFECTOS"
//lstr_info.copias	= 1
//
//OpenWithParm(vinf,lstr_info)
//
//IF cbx_variedad.Checked Then
//	vinf.dw_1.DataObject = "dw_info_spro_danosydefectos_especie"
//   vinf.dw_1.SetTransObject(sqlca)
//	ll_Fila	= vinf.dw_1.Retrieve(Integer(istr_Mant.Argumento[1]))
//   
//ELSE
//	vinf.dw_1.DataObject = "dw_info_spro_danosydefectos"
//   vinf.dw_1.SetTransObject(sqlca)
//   ll_fila	= vinf.dw_1.Retrieve(Integer(istr_Mant.Argumento[1]), &
//									 Integer(istr_Mant.Argumento[2]))
//END IF								 
//
//IF ll_fila = -1 THEN
//	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
//					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
//ELSEIF ll_fila = 0 THEN
//	MessageBox( "No Existe información", "No existe información para este informe.", &
//					StopSign!, Ok!)
//ELSE
//	F_Membrete(vinf.dw_1)
//	vinf.dw_1.Modify('DataWindow.Print.Preview = Yes')
//	vinf.dw_1.Modify('DataWindow.Print.Preview.Zoom = 75')
//
//	vinf.Visible	= True
//	vinf.Enabled	= True
//END IF
//
//SetPointer(Arrow!)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Integer li_Null
long ll_fila, respuesta

SetNull(li_Null)

DO
	ll_Fila	= dw_1.Retrieve(Integer(istr_Mant.Argumento[1]))
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
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
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

on w_mant_mues_romanas.create
int iCurrent
call super::create
this.st_1=create st_1
this.dw_planta=create dw_planta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.dw_planta
end on

on w_mant_mues_romanas.destroy
call super::destroy
destroy(this.st_1)
destroy(this.dw_planta)
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

event open;call super::open;/*
Argumentos
*********************************************
istr_Mant.Argumento[1]	=> Planta
*********************************************
*/

istr_Mant.Argumento[1]	=""

dw_Planta.GetChild("plde_codigo", idwc_Planta)
idwc_Planta.SetTransObject(SqlCa)
IF idwc_Planta.Retrieve() = 0 THEN
	dw_Planta.Enabled	=	False
ELSE
	dw_Planta.SetTransObject(SqlCa)
	dw_Planta.InsertRow(0)
END IF

iuo_plantadesp	=	Create	uo_plantadesp
buscar	= "Nombre Equipo:Scrpl_equcon"
ordenar	= "Nombre Equipo:Scrpl_equcon"

end event

event ue_modifica();call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False
	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_romanas
integer width = 4421
integer height = 1420
string dataobject = "dw_mues_romanas"
boolean hscrollbar = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_romanas
integer width = 4421
integer height = 240
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_romanas
integer x = 4581
integer y = 52
boolean enabled = false
end type

event pb_lectura::clicked;dw_Planta.Enabled	=	False

Parent.PostEvent("ue_recuperadatos")
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_romanas
integer x = 4581
integer y = 368
end type

event pb_nuevo::clicked;call super::clicked;Integer li_Null

SetNull(li_Null)

dw_Planta.Enabled	=	True

dw_Planta.setitem(1,'plde_codigo',li_Null)

pb_lectura.Enabled	=	False

pb_insertar.Enabled	= 	False
pb_eliminar.Enabled	= 	False
pb_grabar.Enabled		= 	False
pb_imprimir.Enabled	= 	False

istr_mant.argumento[1]=""


dw_Planta.SetFocus()
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_romanas
integer x = 4581
integer y = 556
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_romanas
integer x = 4581
integer y = 724
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_romanas
integer x = 4581
integer y = 892
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_romanas
integer x = 4581
integer y = 1060
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_romanas
integer x = 4581
integer y = 1432
end type

type st_1 from statictext within w_mant_mues_romanas
integer x = 1705
integer y = 152
integer width = 247
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type dw_planta from datawindow within w_mant_mues_romanas
string tag = "Código de Especie"
integer x = 2053
integer y = 144
integer width = 882
integer height = 92
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_planta"
boolean border = false
boolean livescroll = true
end type

event itemchanged;Integer li_Null

SetNull(li_Null)

IF Not iuo_plantadesp.Existe(Integer(data),True,Sqlca) THEN
	This.Object.plde_codigo =  li_Null
	This.SetFocus()
	RETURN 1
ELSE
	istr_mant.argumento[1] = data
	pb_lectura.Enabled	=	True  
END IF 
end event

