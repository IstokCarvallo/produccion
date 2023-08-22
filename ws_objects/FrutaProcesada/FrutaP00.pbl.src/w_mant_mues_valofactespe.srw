$PBExportHeader$w_mant_mues_valofactespe.srw
$PBExportComments$Mantención Valores de Facturación por Productor.
forward
global type w_mant_mues_valofactespe from w_mant_tabla
end type
type dw_cliente from datawindow within w_mant_mues_valofactespe
end type
type st_5 from statictext within w_mant_mues_valofactespe
end type
type dw_zona from datawindow within w_mant_mues_valofactespe
end type
type st_1 from statictext within w_mant_mues_valofactespe
end type
type str_anexos from structure within w_mant_mues_valofactespe
end type
end forward

type str_anexos from structure
	string		titulo
	string		nominf
	string		nomdw
end type

global type w_mant_mues_valofactespe from w_mant_tabla
integer width = 3131
string title = "VALORES DE FACTURACION POR ESPECIE"
dw_cliente dw_cliente
st_5 st_5
dw_zona dw_zona
st_1 st_1
end type
global w_mant_mues_valofactespe w_mant_mues_valofactespe

type variables
w_mant_deta_valofactespe	iw_mantencion
DataWindowChild				idwc_especie
end variables

forward prototypes
public function boolean noexistecliente (integer ai_cliente)
end prototypes

public function boolean noexistecliente (integer ai_cliente);String		ls_Nombre

SELECT	clie_nombre
	INTO	:ls_Nombre
	FROM	"dba"."clientesprod"
	WHERE	clie_codigo	=	:ai_Cliente ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Clientes Producción")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Cliente no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro Código.")
					
	RETURN True
ELSE
	RETURN False
END IF
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

event ue_imprimir;SetPointer(HourGlass!)

Long		fila

istr_info.titulo	= "Valores de Facturación por Especie"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)

vinf.dw_1.DataObject = "dw_info_valofactespe"

vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]))

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

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta
DO
	ll_fila	= dw_1.Retrieve(Integer(istr_mant.argumento[1]), Integer(istr_mant.argumento[2]))
	
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

IF respuesta = 2 THEN
	Close(This)
ELSE
	pb_insertar.Enabled	= True
END IF
end event

on w_mant_mues_valofactespe.create
int iCurrent
call super::create
this.dw_cliente=create dw_cliente
this.st_5=create st_5
this.dw_zona=create dw_zona
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_cliente
this.Control[iCurrent+2]=this.st_5
this.Control[iCurrent+3]=this.dw_zona
this.Control[iCurrent+4]=this.st_1
end on

on w_mant_mues_valofactespe.destroy
call super::destroy
destroy(this.dw_cliente)
destroy(this.st_5)
destroy(this.dw_zona)
destroy(this.st_1)
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

event open;GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, &
							This.Title, "Acceso a Aplicación", 1)
							//
end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_fila, li_Cliente
Integer	li_secuencia

li_Cliente	=	Integer(istr_mant.Argumento[1])

SELECT	"Max"(vafe_secuen)
	INTO	:li_secuencia
	FROM	"dba"."valofactespe"
	WHERE	clie_codigo	=	:li_Cliente ;

IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Valores de Facturación por Especie")
	
	Message.DoubleParm	=	-1
ELSEIF sqlca.SQLCode = 100 OR IsNull(li_secuencia) THEN
	li_secuencia	=	0
END IF

FOR ll_fila	= 1 to dw_1.RowCount()
	IF IsNull(dw_1.Object.vafe_secuen[ll_fila]) OR dw_1.Object.vafe_secuen[ll_fila] = 0 THEN
		li_secuencia ++
		dw_1.Object.vafe_secuen[ll_fila]	= li_secuencia
	END IF
NEXT
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_valofactespe
integer y = 440
integer width = 2578
integer height = 1176
integer taborder = 50
string dataobject = "dw_mues_valofactespe"
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_valofactespe
integer x = 87
integer y = 64
integer width = 2578
integer height = 344
long bordercolor = 33543637
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_valofactespe
integer x = 2793
integer y = 88
integer taborder = 40
end type

event pb_lectura::clicked;call super::clicked;dw_cliente.Enabled	= False
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_valofactespe
integer x = 2793
integer y = 544
integer taborder = 60
string powertiptext = "Nuevo Ingreso"
end type

event pb_nuevo::clicked;call super::clicked;dw_cliente.Enabled	=	True
dw_cliente.SetFocus()
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_valofactespe
integer x = 2793
integer y = 720
integer taborder = 70
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_valofactespe
integer x = 2793
integer y = 896
integer taborder = 80
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_valofactespe
integer x = 2793
integer y = 1072
integer taborder = 90
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_valofactespe
integer x = 2793
integer y = 1248
integer taborder = 100
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_valofactespe
integer x = 2793
integer y = 1624
integer taborder = 110
end type

type dw_cliente from datawindow within w_mant_mues_valofactespe
integer x = 562
integer y = 132
integer width = 1225
integer height = 92
integer taborder = 20
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;IF NoExisteCliente(Integer(data)) THEN
	This.SetItem(1, "clie_codigo", gi_codexport)
	RETURN 1
ELSE
	istr_mant.Argumento[1]	=	data
END IF
end event

type st_5 from statictext within w_mant_mues_valofactespe
integer x = 187
integer y = 144
integer width = 274
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type dw_zona from datawindow within w_mant_mues_valofactespe
integer x = 562
integer y = 248
integer width = 850
integer height = 92
integer taborder = 30
boolean bringtotop = true
string title = "none"
string dataobject = "dddw_zonas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.Argumento[2]	=	data
end event

type st_1 from statictext within w_mant_mues_valofactespe
integer x = 187
integer y = 260
integer width = 274
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 553648127
string text = "Zona"
boolean focusrectangle = false
end type

