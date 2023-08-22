$PBExportHeader$w_mant_mues_ctlcaldefectosverifica.srw
$PBExportComments$Mantenedor de Daños por Especie.
forward
global type w_mant_mues_ctlcaldefectosverifica from w_mant_tabla
end type
type st_1 from statictext within w_mant_mues_ctlcaldefectosverifica
end type
type uo_selespecie from uo_seleccion_especie within w_mant_mues_ctlcaldefectosverifica
end type
end forward

global type w_mant_mues_ctlcaldefectosverifica from w_mant_tabla
string tag = "Muestra daños por especie de tabla CTLCALDANOESPECIE"
integer width = 2738
integer height = 1912
string title = "MAESTRO DISTRIBUCION CALIBRE"
st_1 st_1
uo_selespecie uo_selespecie
end type
global w_mant_mues_ctlcaldefectosverifica w_mant_mues_ctlcaldefectosverifica

type variables
w_mant_deta_defectosverifica  iw_mantencion

Long		li_Especie
end variables

on w_mant_mues_ctlcaldefectosverifica.create
int iCurrent
call super::create
this.st_1=create st_1
this.uo_selespecie=create uo_selespecie
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.uo_selespecie
end on

on w_mant_mues_ctlcaldefectosverifica.destroy
call super::destroy
destroy(this.st_1)
destroy(this.uo_selespecie)
end on

event open;call super::open;Boolean	lb_Cerrar

If IsNull(uo_SelEspecie.Codigo)		Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else	
	li_Especie = Message.DoubleParm
	uo_SelEspecie.Seleccion(False, False)
	
	If li_Especie <> 0 Then
		uo_SelEspecie.Bloquear(True)
		uo_SelEspecie.Codigo	=	li_Especie
		uo_SelEspecie.dw_Seleccion.Object.Codigo[1]=	li_Especie
	End If
	
	istr_mant.dw	= dw_1
	
	istr_mant.argumento[1]	=	String(gi_CodExport)
	istr_mant.argumento[2]	=	"0"
	
	buscar	=	""
	ordenar	=	""
End If
end event

event ue_recuperadatos;Long		ll_fila, respuesta

DO	
	ll_fila	=	dw_1.Retrieve(uo_SelEspecie.Codigo)
		
	IF ll_fila 		=	-1 THEN
		respuesta 	=	MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila >= 0 THEN
		dw_1.SetFocus()
		il_fila					= 1
		pb_imprimir.Enabled	= True
		pb_insertar.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	END IF

LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_borrar();IF dw_1.rowcount() < 1 THEN RETURN

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

event ue_imprimir;Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO DEFECTOS VERIFICACION"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_defectosverifica"
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
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF
end event

event ue_modifica();call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True

istr_Mant.Argumento[2] = String(uo_SelEspecie.Codigo)

OpenWithParm(iw_mantencion, istr_mant)

IF dw_1.RowCount() > 0 and pb_eliminar.Enabled = FALSE THEN
	pb_eliminar.Enabled	= TRUE
	pb_grabar.Enabled		= TRUE
END IF

dw_1.SetRow(il_fila)
dw_1.SelectRow(il_fila,True)

end event

event ue_antesguardar;call super::ue_antesguardar;//Integer	ll_Fila, ll_Secuen
//
//SELECT	IsNull(MAX(ccdc_secuen),0) + 1
//	INTO	:ll_Secuen
//	FROM	dba.ctlcaldistcalibre
//	WHERE	espe_codigo	=	:uo_SelEspecie.Codigo
//	USING SQLCA;
//
//FOR ll_Fila = 1 TO dw_1.RowCount()
//	IF dw_1.GetItemStatus(ll_fila, 0, Primary!) = NewModified! THEN
//		dw_1.Object.ccdc_secuen[ll_Fila] = ll_Secuen
//		ll_Secuen++
//	END IF 
//NEXT
//
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_ctlcaldefectosverifica
integer y = 356
integer width = 2080
integer height = 1376
integer taborder = 60
string dataobject = "dw_mues_defectosverifica"
boolean hscrollbar = true
end type

event dw_1::rowfocuschanged;Integer li_Fila

li_Fila = dw_1.RowCount()

IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila				=	GetRow()
	pb_grabar.Enabled	=	True
END IF
end event

event dw_1::sqlpreview;//
end event

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_ctlcaldefectosverifica
integer y = 72
integer width = 2080
integer height = 252
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_ctlcaldefectosverifica
integer x = 2354
integer y = 88
end type

event pb_lectura::clicked;call super::clicked;uo_SelEspecie.Bloquear(True)

end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_ctlcaldefectosverifica
integer x = 2350
integer y = 384
integer taborder = 30
end type

event pb_nuevo::clicked;call super::clicked;If li_Especie = 0 Then
	uo_SelEspecie.Bloquear(False)
	uo_SelEspecie.LimpiarDatos()
End If

pb_insertar.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled		= False
pb_imprimir.Enabled	= False

il_fila					= 0


end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_ctlcaldefectosverifica
integer x = 2350
integer y = 564
integer taborder = 80
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_ctlcaldefectosverifica
integer x = 2350
integer y = 744
integer taborder = 90
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_ctlcaldefectosverifica
integer x = 2350
integer y = 924
integer taborder = 100
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_ctlcaldefectosverifica
integer x = 2350
integer y = 1104
integer taborder = 110
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_ctlcaldefectosverifica
integer x = 2350
integer y = 1488
integer taborder = 120
end type

type st_1 from statictext within w_mant_mues_ctlcaldefectosverifica
integer x = 425
integer y = 156
integer width = 315
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Especie"
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_mant_mues_ctlcaldefectosverifica
event destroy ( )
integer x = 736
integer y = 148
integer height = 80
integer taborder = 40
boolean bringtotop = true
long backcolor = 33543637
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

