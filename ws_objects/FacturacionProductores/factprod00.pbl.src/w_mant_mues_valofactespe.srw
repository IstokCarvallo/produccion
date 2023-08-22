$PBExportHeader$w_mant_mues_valofactespe.srw
$PBExportComments$Mantención Valores de Facturación por Productor.
forward
global type w_mant_mues_valofactespe from w_mant_tabla
end type
type st_5 from statictext within w_mant_mues_valofactespe
end type
type st_1 from statictext within w_mant_mues_valofactespe
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_valofactespe
end type
type uo_selzonas from uo_seleccion_zonas within w_mant_mues_valofactespe
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
integer width = 3835
string title = "VALORES DE FACTURACION POR ESPECIE"
st_5 st_5
st_1 st_1
uo_selcliente uo_selcliente
uo_selzonas uo_selzonas
end type
global w_mant_mues_valofactespe w_mant_mues_valofactespe

type variables
w_mant_deta_valofactespe	iw_mantencion

DataWindowChild				idwc_especie
end variables

event ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True

istr_mant.argumento[1] 	= 	String(uo_SelCliente.Codigo)
istr_mant.argumento[2] 	= 	String(uo_SelZonas.Codigo)

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
fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelZonas.Codigo)

IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

DO
	ll_fila	= dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelZonas.Codigo)
	
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

IF respuesta = 2 THEN
	Close(This)
ELSE
	pb_insertar.Enabled	= True
END IF
end event

on w_mant_mues_valofactespe.create
int iCurrent
call super::create
this.st_5=create st_5
this.st_1=create st_1
this.uo_selcliente=create uo_selcliente
this.uo_selzonas=create uo_selzonas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_5
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.uo_selcliente
this.Control[iCurrent+4]=this.uo_selzonas
end on

on w_mant_mues_valofactespe.destroy
call super::destroy
destroy(this.st_5)
destroy(this.st_1)
destroy(this.uo_selcliente)
destroy(this.uo_selzonas)
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

istr_mant.argumento[1] 	= 	String(uo_SelCliente.Codigo)
istr_mant.argumento[2] 	= 	String(uo_SelZonas.Codigo)


istr_mant.argumento[10] 	= 	String(dw_1.Object.espe_codigo[il_Fila])
istr_mant.argumento[11] 	= 	String(dw_1.Object.vari_codigo[il_Fila])

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

event open;call super::open;Boolean	lb_Cerrar = False

If IsNull(uo_SelCliente.Codigo) Then lb_Cerrar = True
If IsNull(uo_SelZonas.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	uo_SelCliente.Seleccion(False, False)
	uo_SelZonas.Seleccion(False, False)
	uo_SelCliente.Inicia(gi_CodExport)
	
	
	dw_1.GetChild("espe_codigo", idwc_especie)
	idwc_especie.SetTransObject(sqlca)
	idwc_especie.Retrieve()
	
	buscar	=	"Código Especie:Nespe_codigo,Código Variedad:Nvari_codigo,Nombre Variedad:Svari_nombre,Calibre:Svaca_calibr"
	ordenar	=	"Código Especie:espe_codigo,Código Variedad:vari_codigo,Nombre Variedad:vari_nombre,Calibre:vaca_calibr"
End If
end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False
	
	istr_mant.argumento[1] 	= 	String(uo_SelCliente.Codigo)
	istr_mant.argumento[2] 	= 	String(uo_SelZonas.Codigo)
	istr_mant.argumento[10] 	= 	String(dw_1.Object.espe_codigo[il_Fila])
	istr_mant.argumento[11] 	= 	String(dw_1.Object.vari_codigo[il_Fila])

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_fila
Integer	li_secuencia

SELECT	Max(vafe_secuen)
	INTO	:li_secuencia
	FROM	dbo.valofactespe
	WHERE	clie_codigo	=	:uo_SelCliente.Codigo ;

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
integer x = 91
integer y = 452
integer width = 3122
integer height = 1300
integer taborder = 50
string dataobject = "dw_mues_valofactespe"
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_valofactespe
integer x = 91
integer width = 3122
integer height = 244
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_valofactespe
integer x = 3351
integer y = 88
integer taborder = 40
end type

event pb_lectura::clicked;call super::clicked;uo_SelCliente.Bloquear(True)
uo_SelZonas.Bloquear(True)
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_valofactespe
integer x = 3351
integer y = 544
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;uo_SelCliente.Bloquear(False)
uo_SelZonas.Bloquear(False)
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_valofactespe
integer x = 3351
integer y = 720
integer taborder = 70
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_valofactespe
integer x = 3351
integer y = 896
integer taborder = 80
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_valofactespe
integer x = 3351
integer y = 1072
integer taborder = 90
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_valofactespe
integer x = 3351
integer y = 1248
integer taborder = 100
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_valofactespe
integer x = 3351
integer y = 1624
integer taborder = 110
end type

type st_5 from statictext within w_mant_mues_valofactespe
integer x = 187
integer y = 152
integer width = 274
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Cliente"
boolean focusrectangle = false
end type

type st_1 from statictext within w_mant_mues_valofactespe
integer x = 1701
integer y = 152
integer width = 274
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Zona"
boolean focusrectangle = false
end type

type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_valofactespe
event destroy ( )
integer x = 562
integer y = 140
integer height = 92
integer taborder = 30
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selzonas from uo_seleccion_zonas within w_mant_mues_valofactespe
event destroy ( )
integer x = 2053
integer y = 140
integer height = 92
integer taborder = 40
boolean bringtotop = true
end type

on uo_selzonas.destroy
call uo_seleccion_zonas::destroy
end on

