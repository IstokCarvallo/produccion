$PBExportHeader$w_mant_mues_certificacion_prod.srw
forward
global type w_mant_mues_certificacion_prod from w_mant_tabla
end type
type uo_selproductor from uo_seleccion_productor within w_mant_mues_certificacion_prod
end type
type st_1 from statictext within w_mant_mues_certificacion_prod
end type
type uo_selpredio from uo_seleccion_predioproduc within w_mant_mues_certificacion_prod
end type
type st_2 from statictext within w_mant_mues_certificacion_prod
end type
end forward

global type w_mant_mues_certificacion_prod from w_mant_tabla
string tag = "Certificaciones Productor"
integer width = 3570
string title = "Certificaciones Productor"
uo_selproductor uo_selproductor
st_1 st_1
uo_selpredio uo_selpredio
st_2 st_2
end type
global w_mant_mues_certificacion_prod w_mant_mues_certificacion_prod

type variables
String	is_Ruta

w_mant_deta_certificados_prod iw_mantencion
uo_imagenes		iuo_Imagenes
end variables

forward prototypes
public function boolean wf_grabaimagenes ()
public subroutine wf_replicacion ()
end prototypes

public function boolean wf_grabaimagenes ();Boolean	lb_Retorno = True
Long		ll_Fila
String		ls_Archivo

If dw_1.RowCount() < 1 Then
	lb_Retorno = False
Else
	For ll_Fila = 1 To dw_1.RowCount()
		ls_Archivo	=	dw_1.Object.cece_rutas[ll_Fila] + dw_1.Object.cece_archiv[ll_Fila]
		
		iuo_Imagenes.GrabaImagen(ls_Archivo, dw_1.Object.prod_codigo[ll_Fila], dw_1.Object.prpr_codigo[ll_Fila], dw_1.Object.espe_codigo[ll_Fila], Sqlca)
	Next
End If

Return lb_Retorno
end function

public subroutine wf_replicacion ();Integer	ll_fila

uo_grabatablas		luo_grabatablas
luo_grabatablas = Create uo_grabatablas

ll_fila = dw_1.GetNextModified(ll_fila, Primary!)

If ll_fila > 0 Then 
	If luo_grabatablas.ExisteReplicaTablas(gi_CodExport) Then
		luo_grabatablas.ReplicaTabla_CertcertificacionProd(dw_1)
	End If
End If

Destroy luo_grabatablas
end subroutine

event ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True
istr_mant.Argumento[1] = String(uo_SelProductor.Codigo)
istr_mant.Argumento[2] = String(uo_SelPredio.Codigo)

OpenWithParm( iw_mantencion, istr_mant)

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

lstr_info.titulo	= "MOVIMIENTOS DE CERTIfICACION PRODUCTOR"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)
vinf.dw_1.DataObject = "dw_info_certIficacion_prod"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(uo_SelProductor.Codigo, uo_SelPredio.Codigo)

If fila = -1 Then
	MessageBox("Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox("No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	vinf.dw_1.ModIfy('DataWindow.Print.Preview = Yes')
	vinf.dw_1.ModIfy('DataWindow.Print.Preview.Zoom = 75')

	vinf.Visible	= True
	vinf.Enabled	= True
End If

SetPointer(Arrow!)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

If IsNUll(uo_SelProductor.Codigo) Or uo_SelProductor.Codigo = -1 Then
	MessageBox('Atencion', 'Falta ingresar el Productor.')
	Return 
End If

If IsNUll(uo_SelPredio.Codigo) Or uo_SelPredio.Codigo = -1 Then
	MessageBox('Atencion', 'Falta ingresar el Predio.')
	Return 
End If

uo_SelProductor.Bloquear(True)
uo_SelPredio.Bloquear(True)

DO
	ll_fila	= dw_1.Retrieve(uo_SelProductor.Codigo, uo_SelPredio.Codigo)
	
	IF ll_fila = -1 THEN
		respuesta = MessageBox(	"Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
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

on w_mant_mues_certificacion_prod.create
int iCurrent
call super::create
this.uo_selproductor=create uo_selproductor
this.st_1=create st_1
this.uo_selpredio=create uo_selpredio
this.st_2=create st_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.uo_selproductor
this.Control[iCurrent+2]=this.st_1
this.Control[iCurrent+3]=this.uo_selpredio
this.Control[iCurrent+4]=this.st_2
end on

on w_mant_mues_certificacion_prod.destroy
call super::destroy
destroy(this.uo_selproductor)
destroy(this.st_1)
destroy(this.uo_selpredio)
destroy(this.st_2)
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
istr_mant.Argumento[1] = String(uo_SelProductor.Codigo)
istr_mant.Argumento[2] = String(uo_SelPredio.Codigo)

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
This.Width	= 3799

If IsNull(uo_SelProductor.Codigo)	Then lb_Cerrar	=	True
If IsNull(uo_SelPredio.Codigo)	Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else 
	uo_SelProductor.Seleccion(False, False)
	uo_SelPredio.Seleccion(False, False)
	uo_SelProductor.Filtra(-1)
	
	iuo_Imagenes	=	Create uo_imagenes
	
	RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, is_Ruta)
	
	buscar	= "Especie:Nespe_codigo,Empresa Certificadora:Ncert_codigo,Protocolo:Nprot_codigo,Estado:Nprec_codigo"
	ordenar	= "Especie:espe_codigo,Empresa Certificadora:cert_codigo,Protocolo:prot_codigo,Estado:prec_codigo"
End If
end event

event ue_modifica;call super::ue_modifica;IF dw_1.RowCount() > 0 THEN
	istr_mant.agrega	= False
	istr_mant.borra	= False
	istr_mant.Argumento[1] = String(uo_SelProductor.Codigo)
	istr_mant.Argumento[2] = String(uo_SelPredio.Codigo)

	OpenWithParm(iw_mantencion, istr_mant)
END IF
end event

event ue_guardar;IF dw_1.AcceptText() = -1 THEN RETURN

SetPointer(HourGlass!)

w_main.SetMicroHelp("Grabando información...")

Message.DoubleParm = 0

TriggerEvent("ue_antesguardar")

IF Message.DoubleParm = -1 THEN RETURN

IF wf_actualiza_db() THEN
	If Not wf_GrabaImagenes() Then MessageBox('Atencion', 'Imagenes no pudieron ser almacenadas', Information!, OK!)
	
	w_main.SetMicroHelp("Información Grabada.")
	pb_imprimir.Enabled	= True
ELSE
	w_main.SetMicroHelp("No se puede Grabar información.")
	Message.DoubleParm = -1
	RETURN
END IF
end event

event resize;call super::resize;st_2.x = st_encabe.width/4 - st_2.width
uo_selproductor.x = st_2.x + st_2.width

st_1.x = st_encabe.Width/2 + st_1.Width 
uo_selpredio.x = st_1.x + st_1.width
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_certificacion_prod
integer y = 372
integer width = 2843
integer height = 1480
string dataobject = "dw_mues_certificacion_prod"
boolean hscrollbar = true
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_certificacion_prod
integer width = 2843
integer height = 272
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_certificacion_prod
integer x = 3049
integer y = 160
end type

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_certificacion_prod
integer x = 3049
integer y = 460
end type

event pb_nuevo::clicked;call super::clicked;uo_SelProductor.Bloquear(False)
uo_SelPredio.Bloquear(False)

uo_SelPredio.Filtra(-1)
uo_SelProductor.LimpiarDatos()
uo_SelPredio.LimpiarDatos()
end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_certificacion_prod
integer x = 3049
integer y = 808
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_certificacion_prod
integer x = 3049
integer y = 632
integer height = 244
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_certificacion_prod
integer x = 3081
integer y = 1016
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_certificacion_prod
integer x = 3049
integer y = 1196
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_certificacion_prod
integer x = 3049
integer y = 1564
end type

type uo_selproductor from uo_seleccion_productor within w_mant_mues_certificacion_prod
integer x = 489
integer y = 156
integer height = 72
integer taborder = 20
boolean bringtotop = true
end type

on uo_selproductor.destroy
call uo_seleccion_productor::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return 

Choose Case This.Codigo
	Case -1, -9
		
	Case Else
		uo_SelPredio.Filtra(This.Codigo)

End Choose 
end event

type st_1 from statictext within w_mant_mues_certificacion_prod
integer x = 1550
integer y = 164
integer width = 283
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
string text = "Predio"
alignment alignment = center!
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selpredio from uo_seleccion_predioproduc within w_mant_mues_certificacion_prod
integer x = 1879
integer y = 156
integer height = 68
integer taborder = 30
boolean bringtotop = true
end type

on uo_selpredio.destroy
call uo_seleccion_predioproduc::destroy
end on

type st_2 from statictext within w_mant_mues_certificacion_prod
integer x = 169
integer y = 164
integer width = 306
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
string text = "Productor"
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

