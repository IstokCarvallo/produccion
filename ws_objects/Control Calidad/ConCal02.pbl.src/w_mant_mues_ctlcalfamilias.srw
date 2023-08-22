$PBExportHeader$w_mant_mues_ctlcalfamilias.srw
$PBExportComments$Mantenedor de Familias según sistema.
forward
global type w_mant_mues_ctlcalfamilias from w_mant_directo
end type
end forward

global type w_mant_mues_ctlcalfamilias from w_mant_directo
string tag = "Mantenedor de tabla CTLCALFAMILIA"
integer width = 2336
integer height = 1784
string title = "MAESTRO FAMILIAS"
end type
global w_mant_mues_ctlcalfamilias w_mant_mues_ctlcalfamilias

type variables
DataWindowChild idwc_1
Boolean 	ib_agrega	
Integer  ii_familia,ii_subfamilia,ii_Grupo,ii_subgrupo,ii_grupodet
end variables

forward prototypes
public function boolean duplicado (integer ai_familia)
end prototypes

public function boolean duplicado (integer ai_familia);Integer 	li_Contador
String	al_Familia

al_Familia	=	String(ai_Familia)

li_Contador	= 	dw_1.Find("ccfa_codigo = " + al_Familia, 1, dw_1.RowCount())

IF li_Contador = 0 OR IsNull(li_Contador) THEN
	
		SELECT	Count(*)
			INTO	:li_Contador
			FROM	dbo.ctlcalfamilias
			WHERE	ccfa_codigo	=	:ai_Familia;		
		
		IF	sqlca.sqlcode	=	-1	THEN
			F_ErrorBaseDatos(sqlca,"Lectura de Tabla CTLCALFAMILIAS")
			RETURN TRUE
		END IF
END IF

IF li_Contador > 0 THEN
	MessageBox("Duplicidad de Datos","Familia ingresada ya existe")
	RETURN TRUE
END IF

RETURN False
	
				  
end function

on w_mant_mues_ctlcalfamilias.create
call super::create
end on

on w_mant_mues_ctlcalfamilias.destroy
call super::destroy
end on

event open;x				= 0
y				= 0
This.Width	= dw_1.width + 540
This.Height	= 1993
im_menu	= m_principal

This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	False
This.Icon									=	Gstr_apl.Icono

dw_1.SetTransObject(sqlca)
dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
dw_1.Modify("DataWindow.Footer.Height = 110")

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)								


buscar			= "Código:Nccfa_codigo,Descripción:Sccfa_descrip"
ordenar			= "Código:ccfa_codigo,Descripción:ccfa_descrip"

pb_grabar.Enabled	=	False
pb_nuevo.Enabled	=	False
end event

event ue_recuperadatos();call super::ue_recuperadatos;Long	ll_fila, respuesta
DO
	ll_fila		 = dw_1.Retrieve()
	
	IF ll_fila 	 = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", &
										Information!, RetryCancel!)
	ELSEIF ll_fila > 0 THEN
		il_fila = 1
		//dw_1.SetRow(1)
		//dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	ELSE
		pb_eliminar.enabled  = False
		pb_insertar.SetFocus()
	END IF
LOOP WHILE respuesta = 1

IF respuesta = 2 THEN Close(This)
end event

event ue_imprimir;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_ctlcalfamilias"

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
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)

	END IF
END IF

SetPointer(Arrow!)
end event

event ue_borrar;IF dw_1.rowcount() < 1 THEN RETURN

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

IF Message.DoubleParm = -1 THEN RETURN

IF MessageBox("Eliminación de Registro", "Está seguro de Eliminar este Registro", Question!, YesNo!) = 1 THEN
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
	END IF
END IF
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


For ll_Fila	= 1 To dw_1.RowCount()
		IF IsNull(dw_1.Object.ccfa_codigo[ll_Fila]) OR dw_1.Object.ccfa_codigo[ll_Fila] = 0 THEN
			li_cont ++
			ls_mensaje 		= ls_mensaje + "~nCódigo"
			ls_colu[li_cont]	= "ccfa_codigo"
		END IF
		
		IF IsNull(dw_1.Object.ccfa_descrip[ll_Fila]) OR trim(dw_1.Object.ccfa_descrip[ll_Fila]) = "" THEN
			li_cont ++
			ls_mensaje 			= ls_mensaje + "~nDescripción"
			ls_colu[li_cont]	= "ccfa_descrip"
		END IF
		
		IF IsNull(dw_1.Object.ccfa_pidesf[ll_Fila]) OR dw_1.Object.ccfa_pidesf[ll_Fila] = 0 THEN
			li_cont ++
			ls_mensaje 			= ls_mensaje + "~nSub Familia"
			ls_colu[li_cont]	= "ccfa_pidesf"
		END IF

Next

	If li_cont > 0 Then
		MessageBox("Error de Consistencia", "Falta el ingreso de :" + ls_mensaje + ".", StopSign!, Ok!)
		dw_1.SetColumn(ls_colu[1])
		dw_1.SetFocus()
		Message.DoubleParm = -1
	else
		Message.DoubleParm = 0
	End If
	
end event

type st_encabe from w_mant_directo`st_encabe within w_mant_mues_ctlcalfamilias
boolean visible = false
integer x = 96
integer y = 1576
integer width = 1801
integer height = 64
end type

type pb_nuevo from w_mant_directo`pb_nuevo within w_mant_mues_ctlcalfamilias
boolean visible = false
integer x = 1979
integer y = 408
boolean enabled = false
string powertiptext = "Nuevo Ingreso"
end type

event pb_nuevo::clicked;call super::clicked;pb_insertar.Enabled		=	False
pb_eliminar.Enabled		=	False
pb_grabar.Enabled			=	False
pb_lectura.Enabled		=	True

end event

type pb_lectura from w_mant_directo`pb_lectura within w_mant_mues_ctlcalfamilias
integer x = 1979
end type

event pb_lectura::clicked;call super::clicked;//pb_lectura.Enabled		=	False
//pb_nuevo.Enabled			=	True
//
end event

type pb_eliminar from w_mant_directo`pb_eliminar within w_mant_mues_ctlcalfamilias
integer x = 1979
integer y = 736
end type

type pb_insertar from w_mant_directo`pb_insertar within w_mant_mues_ctlcalfamilias
integer x = 1979
integer y = 572
end type

type pb_salir from w_mant_directo`pb_salir within w_mant_mues_ctlcalfamilias
integer x = 1989
integer y = 1348
end type

type pb_imprimir from w_mant_directo`pb_imprimir within w_mant_mues_ctlcalfamilias
integer x = 1979
integer y = 1064
end type

type pb_grabar from w_mant_directo`pb_grabar within w_mant_mues_ctlcalfamilias
integer x = 1979
integer y = 900
end type

type dw_1 from w_mant_directo`dw_1 within w_mant_mues_ctlcalfamilias
integer x = 64
integer y = 64
integer width = 1801
integer height = 1476
string dataobject = "dw_mues_ctlcalfamilias"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

event dw_1::rowfocuschanged;call super::rowfocuschanged;Integer li_Fila

li_Fila = dw_1.RowCount()

//IF li_Fila > 0 THEN
//	dw_1.SetItem(li_Fila, "ccfa_codigo", 0)
//END IF

IF RowCount() < 1 OR GetRow() = 0 OR ib_borrar THEN
	ib_datos_ok = False
ELSE
	il_fila				=	GetRow()
	pb_grabar.Enabled	=	True
END IF

end event

event dw_1::clicked;call super::clicked;ii_familia	=	dw_1.Object.ccfa_codigo[il_Fila]
end event

event dw_1::itemchanged;Long			ll_canfila
Integer		li_codigo
String	 	ls_Columna

ls_Columna	=	dwo.Name
ll_canfila	=	dw_1.RowCount()
li_codigo	=	Integer(data)

CHOOSE CASE ls_Columna
	CASE "ccfa_codigo"
		IF il_fila <= ll_canfila - 1 THEN
			li_codigo=ii_familia
			IF	Duplicado(li_codigo)	THEN
				dw_1.Object.ccfa_codigo[il_fila]=ii_familia
			END IF

		ELSE
			
			IF	Duplicado(li_codigo)	THEN
				dw_1.DeleteRow(il_Fila)
			END IF
   	END IF

END CHOOSE
end event

