$PBExportHeader$w_mant_mues_ctlcalagroproduc.srw
$PBExportComments$Mantenedor de Asociación de Agrónomos / Productores
forward
global type w_mant_mues_ctlcalagroproduc from w_mant_tabla
end type
type dw_zona from datawindow within w_mant_mues_ctlcalagroproduc
end type
type st_2 from statictext within w_mant_mues_ctlcalagroproduc
end type
type st_1 from statictext within w_mant_mues_ctlcalagroproduc
end type
end forward

global type w_mant_mues_ctlcalagroproduc from w_mant_tabla
integer width = 3579
string title = "PRODUCTORES / AGRONOMOS"
dw_zona dw_zona
st_2 st_2
st_1 st_1
end type
global w_mant_mues_ctlcalagroproduc w_mant_mues_ctlcalagroproduc

type variables
w_mant_deta_ctlcalagroproduc	iw_mantencion

DataWindowChild  idwc_zona
end variables

forward prototypes
public function boolean noexistezona (integer ai_zona)
public function boolean noexisteproductor (long al_codigo)
end prototypes

public function boolean noexistezona (integer ai_zona);String ls_nombre_zona

SELECT	zona_nombre
	INTO	:ls_nombre_zona
	FROM	dba.zonas
  WHERE	zona_codigo	=	:ai_zona;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla ZONAS")
	
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Zona no ha sido Definido.~r~r" + &
					"Ingrese o seleccione otro código.")	
	RETURN True
END IF	

RETURN False
end function

public function boolean noexisteproductor (long al_codigo);String	ls_Nombre
Integer	li_Cliente

li_Cliente	=	Integer(istr_mant.Argumento[1])

SELECT	prod_nombre
	INTO	:ls_Nombre
	FROM	dba.productores
	WHERE	prod_codigo	=	:al_Codigo ;
	
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura tabla Productores")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Productor no ha sido Ingresado.~r~r" + &
					"Ingrese o Seleccione otro Productor")
	RETURN True
ELSE
	istr_mant.Argumento[2]	=	String(al_codigo)
	istr_mant.Argumento[3]	=	ls_Nombre
	
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
str_info	lstr_info

lstr_info.titulo	= "PRODUCTORES / AGRONOMOS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

vinf.dw_1.DataObject = "dw_info_ctlcalagroproduc"

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
	ll_fila	= dw_1.Retrieve(0,Integer(istr_mant.argumento[1]))
	
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

on w_mant_mues_ctlcalagroproduc.create
int iCurrent
call super::create
this.dw_zona=create dw_zona
this.st_2=create st_2
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_zona
this.Control[iCurrent+2]=this.st_2
this.Control[iCurrent+3]=this.st_1
end on

on w_mant_mues_ctlcalagroproduc.destroy
call super::destroy
destroy(this.dw_zona)
destroy(this.st_2)
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

event open;call super::open;dw_zona.GetChild("zona_codigo", idwc_zona)
idwc_zona.SetTransObject(sqlca)
dw_zona.InsertRow(0)

IF idwc_zona.Retrieve()	=	0	THEN
	idwc_zona.InsertRow(0)
	dw_zona.SetTransObject(sqlca)
END IF


pb_lectura.Enabled	=	False

buscar	= "Agrónomo:Sccag_nombre,Cod. Cliente:Nclie_codigo,Productor:Sprod_nombre,Nro. Lote:Nccle_nrolot"
ordenar	= "Agrónomo:ccag_nombre,Cod. Cliente:clie_codigo,Productor:prod_nombre,Nro. Lote:ccle_nrolot"

end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_ctlcalagroproduc
integer x = 73
integer y = 356
integer width = 2880
integer height = 1348
integer taborder = 50
string dataobject = "dw_mues_ctlcalagroproduc"
boolean hscrollbar = true
boolean hsplitscroll = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_ctlcalagroproduc
boolean visible = false
integer x = 73
integer y = 72
integer width = 2880
integer height = 224
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_ctlcalagroproduc
integer x = 3269
integer y = 112
integer taborder = 30
end type

event pb_lectura::clicked;call super::clicked;dw_zona.Enabled	= False
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_ctlcalagroproduc
integer x = 3269
integer y = 440
integer taborder = 40
end type

event pb_nuevo::clicked;call super::clicked;dw_zona.Reset()

dw_zona.GetChild("zona_codigo", idwc_zona)
idwc_zona.SetTransObject(sqlca)
idwc_zona.Retrieve()
dw_zona.InsertRow(0)

dw_zona.Enabled	= True
dw_zona.SetFocus()

pb_lectura.Enabled	= False
pb_insertar.Enabled	= False
pb_eliminar.Enabled	= False
pb_grabar.Enabled		= False
pb_imprimir.Enabled	= False

il_fila					= 0

end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_ctlcalagroproduc
integer x = 3269
integer y = 616
integer taborder = 60
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_ctlcalagroproduc
integer x = 3269
integer y = 784
integer taborder = 70
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_ctlcalagroproduc
integer x = 3269
integer y = 952
integer taborder = 80
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_ctlcalagroproduc
integer x = 3269
integer y = 1120
integer taborder = 90
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_ctlcalagroproduc
integer x = 3269
integer y = 1492
integer taborder = 100
end type

type dw_zona from datawindow within w_mant_mues_ctlcalagroproduc
integer x = 1166
integer y = 136
integer width = 850
integer height = 92
integer taborder = 10
boolean bringtotop = true
string dataobject = "dddw_zonas"
boolean border = false
boolean livescroll = true
end type

event itemchanged;
Long ll_Null

SetNull(ll_Null)

IF Noexistezona(integer(data)) THEN	
	This.SetItem(row, "zona_codigo", ll_Null)
   pb_lectura.Enabled		=	FALSE
	RETURN 1 	
ELSE	

	istr_mant.argumento[1]	=  Data
	pb_lectura.Enabled	   =	True
	pb_lectura.SetFocus()
END IF
end event

event itemerror;RETURN 1 
end event

type st_2 from statictext within w_mant_mues_ctlcalagroproduc
integer x = 919
integer y = 144
integer width = 229
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 553648127
boolean enabled = false
string text = "Zona"
boolean focusrectangle = false
end type

type st_1 from statictext within w_mant_mues_ctlcalagroproduc
integer x = 73
integer y = 72
integer width = 2546
integer height = 224
integer textsize = -10
integer weight = 400
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 12632256
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

