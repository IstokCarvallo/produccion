$PBExportHeader$w_mant_mues_pmg_exportadora.srw
$PBExportComments$Mantención Valores de Facturación por Productor.
forward
global type w_mant_mues_pmg_exportadora from w_mant_tabla
end type
type st_2 from statictext within w_mant_mues_pmg_exportadora
end type
type st_5 from statictext within w_mant_mues_pmg_exportadora
end type
type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_pmg_exportadora
end type
type uo_selzonas from uo_seleccion_zonas within w_mant_mues_pmg_exportadora
end type
type str_anexos from structure within w_mant_mues_pmg_exportadora
end type
end forward

type str_anexos from structure
	string		titulo
	string		nominf
	string		nomdw
end type

global type w_mant_mues_pmg_exportadora from w_mant_tabla
integer width = 3785
integer height = 1936
string title = "VALORES DE FACTURACION POR PRODUCTOR"
st_2 st_2
st_5 st_5
uo_selcliente uo_selcliente
uo_selzonas uo_selzonas
end type
global w_mant_mues_pmg_exportadora w_mant_mues_pmg_exportadora

type variables
w_mant_deta_pmg_exportadora iw_mantencion

DataWindowChild				idwc_especie
end variables

event ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True

istr_Mant.Argumento[1] = String(uo_SelCliente.Codigo)
istr_Mant.Argumento[2] = String(uo_SelZonas.Codigo)
istr_Mant.Argumento[3] = uo_SelZonas.Nombre

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

istr_info.titulo	= "Valores de Facturación PMG Exportadora"
istr_info.copias	= 1

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_info_pmg_exportadora"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelZonas.Codigo)

If fila = -1 Then
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf fila = 0 Then
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

event ue_recuperadatos;call super::ue_recuperadatos;Long	ll_fila, respuesta

Do
	ll_fila	= dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelZonas.Codigo)
	
	If ll_fila = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ElseIf ll_fila > 0 Then
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled		= True
	Else
		pb_insertar.SetFocus()
	End If
Loop While respuesta = 1

If respuesta = 2 Then
	Close(This)
Else
	pb_insertar.Enabled	= True
End If
end event

on w_mant_mues_pmg_exportadora.create
int iCurrent
call super::create
this.st_2=create st_2
this.st_5=create st_5
this.uo_selcliente=create uo_selcliente
this.uo_selzonas=create uo_selzonas
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_5
this.Control[iCurrent+3]=this.uo_selcliente
this.Control[iCurrent+4]=this.uo_selzonas
end on

on w_mant_mues_pmg_exportadora.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_5)
destroy(this.uo_selcliente)
destroy(this.uo_selzonas)
end on

event ue_borrar;If dw_1.rowcount() < 1 Then Return

SetPointer(HourGlass!)

ib_borrar = True
w_main.SetMicroHelp("Validando la eliminación...")

Message.DoubleParm = 0

This.TriggerEvent ("ue_validaborrar")

If Message.DoubleParm = -1 Then Return

istr_mant.borra	= True
istr_mant.agrega	= False

istr_Mant.Argumento[1] = String(uo_SelCliente.Codigo)
istr_Mant.Argumento[2] = String(uo_SelZonas.Codigo)
istr_Mant.Argumento[3] = uo_SelZonas.Nombre

OpenWithParm(iw_mantencion, istr_mant)

istr_mant = Message.PowerObjectParm

If istr_mant.respuesta = 1 Then
	If dw_1.DeleteRow(0) = 1 Then
		ib_borrar = False
		w_main.SetMicroHelp("Borrando Registro...")
		SetPointer(Arrow!)
	Else
		ib_borrar = False
		MessageBox(This.Title,"No se puede borrar actual registro.")
	End If
	
	If dw_1.RowCount() = 0 Then
		pb_eliminar.Enabled = False
	Else
		il_fila = dw_1.GetRow()
		dw_1.SelectRow(il_fila,True)
	End If
End If

istr_mant.borra	 = False
end event

event open;call super::open;Boolean lb_Cerrar = False

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

event ue_modifica;If dw_1.RowCount() > 0 Then
	istr_mant.agrega	= False
	istr_mant.borra	= False
	istr_Mant.Argumento[1] = String(uo_SelCliente.Codigo)
	istr_Mant.Argumento[2] = String(uo_SelZonas.Codigo)
	istr_Mant.Argumento[3] = uo_SelZonas.Nombre
	
	OpenWithParm(iw_mantencion, istr_mant)
End If
end event

event ue_antesguardar;call super::ue_antesguardar;Long		ll_fila, ll_productor
Integer	li_secuencia



SELECT	Max(vafa_secuen)
	INTO	:li_secuencia
	FROM	dbo.PMGExportadora
	WHERE	clie_codigo	=	:uo_SelCliente.Codigo
	AND   zona_codigo =  :uo_SelZonas.Codigo;

If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca, "Lectura de tabla Valores de Facturación por PMG Exportadora")
	Message.DoubleParm	=	-1
ElseIf sqlca.SQLCode = 100 OR IsNull(li_secuencia) Then
	li_secuencia	=	0
End If

For ll_fila	= 1 To dw_1.RowCount()
	If IsNull(dw_1.Object.vafa_secuen[ll_fila]) OR dw_1.Object.vafa_secuen[ll_fila] = 0 Then
		li_secuencia ++
		dw_1.Object.vafa_secuen[ll_fila]	= li_secuencia
	End If
Next
end event

type dw_1 from w_mant_tabla`dw_1 within w_mant_mues_pmg_exportadora
integer y = 364
integer width = 3003
integer height = 1436
integer taborder = 50
string dataobject = "dw_mues_pmg_exportadora"
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_pmg_exportadora
integer width = 3003
integer height = 260
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_pmg_exportadora
integer x = 3392
integer y = 108
integer taborder = 40
end type

event pb_lectura::clicked;call super::clicked;uo_SelCliente.Bloquear(True)
uo_SelZonas.Bloquear(True)
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_pmg_exportadora
integer x = 3392
integer y = 564
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;uo_SelCliente.Bloquear(False)
uo_SelZonas.Bloquear(False)

end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_pmg_exportadora
integer x = 3392
integer y = 740
integer taborder = 70
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_pmg_exportadora
integer x = 3392
integer y = 916
integer taborder = 80
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_pmg_exportadora
integer x = 3392
integer y = 1092
integer taborder = 90
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_pmg_exportadora
integer x = 3392
integer y = 1268
integer taborder = 100
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_pmg_exportadora
integer x = 3392
integer y = 1572
integer taborder = 110
end type

type st_2 from statictext within w_mant_mues_pmg_exportadora
integer x = 1719
integer y = 152
integer width = 293
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Zona"
boolean focusrectangle = false
end type

type st_5 from statictext within w_mant_mues_pmg_exportadora
integer x = 187
integer y = 160
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

type uo_selcliente from uo_seleccion_clientesprod within w_mant_mues_pmg_exportadora
event destroy ( )
integer x = 567
integer y = 148
integer height = 92
integer taborder = 30
boolean bringtotop = true
end type

on uo_selcliente.destroy
call uo_seleccion_clientesprod::destroy
end on

type uo_selzonas from uo_seleccion_zonas within w_mant_mues_pmg_exportadora
event destroy ( )
integer x = 2053
integer y = 148
integer height = 92
integer taborder = 50
boolean bringtotop = true
end type

on uo_selzonas.destroy
call uo_seleccion_zonas::destroy
end on

