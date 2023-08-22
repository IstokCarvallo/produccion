$PBExportHeader$w_mant_mues_ctlcaldefectos.srw
$PBExportComments$Mantenedor de Defectos Generales.
forward
global type w_mant_mues_ctlcaldefectos from w_mant_directo
end type
type st_4 from statictext within w_mant_mues_ctlcaldefectos
end type
type st_1 from statictext within w_mant_mues_ctlcaldefectos
end type
type uo_selespecie from uo_seleccion_especie within w_mant_mues_ctlcaldefectos
end type
type uo_selfamilia from uo_seleccion_familia within w_mant_mues_ctlcaldefectos
end type
end forward

global type w_mant_mues_ctlcaldefectos from w_mant_directo
string tag = "Maestro de Defectos Recepción"
integer width = 2409
integer height = 1900
string title = "Maestro de Defectos Recepción"
st_4 st_4
st_1 st_1
uo_selespecie uo_selespecie
uo_selfamilia uo_selfamilia
end type
global w_mant_mues_ctlcaldefectos w_mant_mues_ctlcaldefectos

type variables
DataWindowChild	idwc_familias,idwc_especie

Integer				ii_tipo, ii_orden

uo_zonas          iuo_zonas
uo_especie			iuo_especies	

end variables

forward prototypes
public function boolean duplicado (string columna, string valor)
public function boolean noexistefamilia (integer ai_familia)
end prototypes

public function boolean duplicado (string columna, string valor);Long		ll_fila
Integer	li_codigo

li_codigo	=	dw_1.Object.ccde_codigo[il_fila]

CHOOSE CASE columna
	CASE "ccde_codigo"
		li_codigo	=	Integer(valor)

END CHOOSE

ll_fila	= dw_1.Find("ccde_codigo = " + String(li_codigo), + &
							1, dw_1.RowCount())

IF ll_fila > 0 and ll_fila <> il_fila THEN
	MessageBox("Error","Registro ya fue ingresado anteriormente",Information!, Ok!)
	RETURN True
ELSE
	RETURN False
END IF

end function

public function boolean noexistefamilia (integer ai_familia);String ls_nombre_familia

SELECT	ccfa_descrip
	INTO	:ls_nombre_familia
	FROM	dbo.ctlcalfamilias
	WHERE	ccfa_codigo	=	:ai_familia AND
	      ccfa_pidesf = 1;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla CTLCALFAMILIAS")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Familia no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro código.")
	
	RETURN True
END IF
RETURN False
end function

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

If IsNull(uo_SelFamilia.Codigo) Or uo_SelFamilia.Codigo = -1 Then
	MessageBox('Atencion', 'Falta ingresar Familia')
	Return
End If

If IsNull(uo_SelEspecie.Codigo) Or uo_SelEspecie.Codigo = -1 Then
	MessageBox('Atencion', 'Falta ingresar Especie')
	Return
End If

uo_SelFamilia.Bloquear(True)


DO
	ll_fila	= dw_1.Retrieve(uo_SelFamilia.Codigo, uo_SelEspecie.Codigo)

	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		dw_1.SetFocus()
		il_fila					= 1
		pb_imprimir.Enabled	= True
		pb_insertar.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_insertar.Enabled	= True
		pb_insertar.SetFocus()
	END IF

LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event open;call super::open;Boolean	lb_Cerrar
Long		li_especie


If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar	=	True
If IsNull(uo_SelFamilia.Codigo) 	Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else	
	li_Especie = Message.DoubleParm
	
	uo_SelEspecie.Seleccion(False, False)
	uo_SelFamilia.Seleccion(False, False)
	If li_Especie <> 0 Then
		uo_SelEspecie.Bloquear(True)
		uo_SelEspecie.Codigo	=	li_Especie
		uo_SelEspecie.dw_Seleccion.Object.Codigo[1]=	li_Especie
	End If

	istr_mant.dw				= dw_1

	buscar			= "Código Defecto:Nccde_codigo,Descripción:Sccde_descri"
	ordenar			= "Código Defecto:ccde_codigo,Descripción:ccde_descri"
End If

end event

on w_mant_mues_ctlcaldefectos.create
int iCurrent
call super::create
this.st_4=create st_4
this.st_1=create st_1
this.uo_selespecie=create uo_selespecie
this.uo_selfamilia=create uo_selfamilia
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.uo_selespecie
this.Control[iCurrent+4]=this.uo_selfamilia
end on

on w_mant_mues_ctlcaldefectos.destroy
call super::destroy
destroy(this.st_4)
destroy(this.st_1)
destroy(this.uo_selespecie)
destroy(this.uo_selfamilia)
end on

event ue_imprimir;SetPointer(HourGlass!)

Integer	li_zona
Long		fila
str_info	lstr_info

lstr_info.titulo	= "MAESTRO DE DEFECTOS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_defectos_recepcion"
vinf.dw_1.SetTransObject(sqlca)


fila = vinf.dw_1.Retrieve(uo_SelFamilia.Codigo, uo_SelEspecie.Codigo)

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

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event ue_antesguardar;Long		ll_fila = 1
Integer	li_cont
String	ls_mensaje, ls_colu[]

DO WHILE ll_fila <= dw_1.RowCount()
	IF dw_1.GetItemStatus(ll_fila, 0, Primary!) = New! THEN
		dw_1.DeleteRow(ll_fila)
	ELSE
		ll_fila ++
	END IF
LOOP

FOR ll_Fila	= 1 TO dw_1.RowCount()
	IF Isnull(dw_1.Object.ccde_codigo[ll_Fila]) OR dw_1.Object.ccde_codigo[ll_Fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCódigo de Defecto"
		ls_colu[li_cont]	= "ccde_codigo"
	END IF
	
	IF Isnull(dw_1.Object.ccde_descri[ll_Fila]) OR trim(dw_1.Object.ccde_descri[ll_Fila]) = "" THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nDescripción del Defecto"
		ls_colu[li_cont]	= "ccde_descri"
	END IF
	
	IF li_cont > 0 THEN
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
		Message.DoubleParm = -1
	END IF
NEXT

FOR ll_Fila	= 1 TO dw_1.RowCount()
	dw_1.Object.ccfa_codigo[ll_Fila] = uo_SelFamilia.Codigo
	dw_1.Object.espe_codigo[ll_Fila] = uo_SelEspecie.Codigo
NEXT

end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_ctlcaldefectos
integer x = 82
integer y = 52
integer width = 1847
integer height = 312
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_ctlcaldefectos
integer x = 2062
integer y = 408
integer taborder = 40
end type

event pb_nuevo::clicked;call super::clicked;uo_SelFamilia.Bloquear(False)
uo_SelFamilia.LimpiarDatos()

pb_insertar.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled	= False
pb_imprimir.Enabled	= False

il_fila					= 0
end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_ctlcaldefectos
integer x = 2062
integer y = 136
integer taborder = 30
end type

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_ctlcaldefectos
integer x = 2062
integer y = 792
integer taborder = 70
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_ctlcaldefectos
integer x = 2062
integer y = 612
integer taborder = 60
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_ctlcaldefectos
integer x = 2062
integer y = 1536
integer taborder = 100
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_ctlcaldefectos
integer x = 2062
integer y = 1152
integer taborder = 90
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_ctlcaldefectos
integer x = 2062
integer y = 972
integer taborder = 80
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_ctlcaldefectos
integer x = 82
integer y = 388
integer width = 1847
integer height = 1292
integer taborder = 50
string dataobject = "dw_mues_ctlcaldefectos"
boolean hscrollbar = true
end type

event dw_1::itemchanged;call super::itemchanged;Long		ll_null

SetNull(ll_null)

CHOOSE CASE dwo.Name
	
	CASE "ccde_codigo"
		IF Duplicado(dwo.Name, data) THEN
			This.SetItem(il_fila, "ccde_codigo", ll_null)
			RETURN 1
		END IF

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

	CASE "ccag_abrevi"
			pb_grabar.Enabled	=	True

//	CASE "cctc_codigo"
//			TriggerEvent("ue_validaregistro")

END CHOOSE

end event

event dw_1::sqlpreview;//
end event

type st_4 from statictext within w_mant_mues_ctlcaldefectos
integer x = 247
integer y = 216
integer width = 233
integer height = 80
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
string text = "Familia"
boolean focusrectangle = false
end type

type st_1 from statictext within w_mant_mues_ctlcaldefectos
integer x = 247
integer y = 120
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

type uo_selespecie from uo_seleccion_especie within w_mant_mues_ctlcaldefectos
event destroy ( )
integer x = 507
integer y = 112
integer height = 80
integer taborder = 110
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type uo_selfamilia from uo_seleccion_familia within w_mant_mues_ctlcaldefectos
event destroy ( )
integer x = 507
integer y = 216
integer height = 80
integer taborder = 50
boolean bringtotop = true
end type

on uo_selfamilia.destroy
call uo_seleccion_familia::destroy
end on

