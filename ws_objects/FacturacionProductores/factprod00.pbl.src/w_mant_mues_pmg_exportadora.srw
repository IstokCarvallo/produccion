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
type cb_carga from commandbutton within w_mant_mues_pmg_exportadora
end type
type dw_carga from datawindow within w_mant_mues_pmg_exportadora
end type
type st_1 from statictext within w_mant_mues_pmg_exportadora
end type
type uo_selespecie from uo_seleccion_especie within w_mant_mues_pmg_exportadora
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
integer width = 3744
integer height = 1936
string title = "VALORES DE FACTURACION EXPORTADORA"
st_2 st_2
st_5 st_5
uo_selcliente uo_selcliente
uo_selzonas uo_selzonas
cb_carga cb_carga
dw_carga dw_carga
st_1 st_1
uo_selespecie uo_selespecie
end type
global w_mant_mues_pmg_exportadora w_mant_mues_pmg_exportadora

type variables
w_mant_deta_pmg_exportadora iw_mantencion

uo_semanafactura	iuo_Semana
end variables

forward prototypes
public function integer wf_cargaarchivo ()
end prototypes

public function integer wf_cargaarchivo ();String	ls_path, ls_File
Long	ll_File, ll_New, ll_Fila, ll_Retorno = 1


OleObject 			loo_Excel
uo_Variedades		iuo_Variedad

iuo_Variedad	=	Create uo_Variedades
loo_Excel			=	Create OleObject 

If GetFileOpenName ( "Integración Proforma", ls_path, ls_File, "XLSX","Excel Files(*.xlsx),*.xlsx" ) < 1 Then Return -1

dw_Carga.Reset()

loo_Excel.ConnectToNewObject( "excel.application" ) 
loo_Excel.Visible = false 
loo_Excel.WorkBooks.Open( ls_Path)
loo_Excel.Application.ActiveWorkbook.Worksheets[1].Activate 
loo_Excel.Application.ActiveWorkbook.Worksheets[1].Range("A1").Activate 
loo_Excel.ActiveCell.CurrentRegion.Select() 
loo_Excel.Selection.Copy() 
dw_Carga.ImportClipBoard (2)

ClipBoard('')
loo_Excel.WorkBooks.Close()
loo_Excel.Application.Quit
loo_Excel.DisconnectObject()

//ll_File = dw_carga.ImportFile(CSV!, ls_File)
	
If ll_File < 0 Then
	MessageBox('Alerta', 'Error en la carga de archivo.', Exclamation!, OK!)
	ll_Retorno =  ll_File
Else
	For ll_Fila = 1 To dw_Carga.RowCount()
		ll_New = dw_1.InsertRow(0)
		
		iuo_Semana.of_Existe(Integer(dw_carga.Object.semana[ll_Fila]), False, Sqlca)
		iuo_Variedad.Existe(Integer(dw_carga.Object.Especie[ll_Fila]), Integer(dw_carga.Object.Variedad[ll_Fila]), False, Sqlca)
		
		dw_1.Object.clie_codigo[ll_New]		=	Integer(dw_carga.Object.Cliente[ll_Fila])
		dw_1.Object.zona_codigo[ll_New]		=	Integer(dw_carga.Object.Zona[ll_Fila])
		dw_1.Object.espe_codigo[ll_New]		=	Integer(dw_carga.Object.Especie[ll_Fila])
		dw_1.Object.vari_codigo[ll_New]		=	Integer(dw_carga.Object.Variedad[ll_Fila])
		dw_1.Object.vari_nombre[ll_New]		=	iuo_Variedad.NombreVariedad
		dw_1.Object.emba_codigo[ll_New]	=	dw_carga.Object.Embalaje[ll_Fila]
		dw_1.Object.emba_tipvid[ll_New]		=	Integer(dw_carga.Object.tipovida[ll_Fila])
		dw_1.Object.vaca_calibr[ll_New]		=	dw_carga.Object.Calibre[ll_Fila]
		dw_1.Object.colo_nombre[ll_New]	=	dw_carga.Object.Color[ll_Fila]
		dw_1.Object.vafp_preuni[ll_New]		=	Dec(dw_carga.Object.Valor[ll_Fila])
		dw_1.Object.semana[ll_New]			=	Integer(dw_carga.Object.semana[ll_Fila])
		dw_1.Object.vafa_fecini[ll_New]		=	iuo_Semana.Desde
		dw_1.Object.vafa_fecter[ll_New]		=	iuo_Semana.Hasta	
		
		dw_1.SetRow(ll_New)
  		dw_1.ScrollToRow(ll_New)
	Next
End If

Destroy iuo_Variedad
Destroy loo_Excel

pb_imprimir.Enabled	= True
pb_eliminar.Enabled	= True
pb_grabar.Enabled	= True

Return ll_Retorno
 
end function

event ue_nuevo;istr_mant.borra	= False
istr_mant.agrega	= True

istr_Mant.Argumento[1] = String(uo_SelCliente.Codigo)
istr_Mant.Argumento[2] = String(uo_SelZonas.Codigo)
istr_Mant.Argumento[3] = uo_SelZonas.Nombre
istr_Mant.Argumento[4] = String(uo_SelEspecie.Codigo)

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
fila = vinf.dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelZonas.Codigo, uo_SelEspecie.Codigo)

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
	ll_fila	= dw_1.Retrieve(uo_SelCliente.Codigo, uo_SelZonas.Codigo, uo_SelEspecie.Codigo)
	
	If ll_fila = -1 Then
		respuesta = MessageBox("Error en Base de Datos", "No es posible conectar la Base de Datos.", Information!, RetryCancel!)
	ElseIf ll_fila > 0 Then
		dw_1.SetRow(1)
		dw_1.SelectRow(1,True)
		dw_1.SetFocus()
		pb_imprimir.Enabled	= True
		pb_eliminar.Enabled	= True
		pb_grabar.Enabled	= True
		cb_carga.Enabled		= True
		
		For ll_Fila = 1 To dw_1.RowCount()
			If iuo_Semana.of_Semana(dw_1.Object.vafa_fecini[ll_Fila], SQLCA) Then
				dw_1.Object.semana[ll_Fila] = iuo_Semana.Semana
			End If
		Next

	Else
		pb_insertar.SetFocus()
		cb_carga.Enabled		= True
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
this.cb_carga=create cb_carga
this.dw_carga=create dw_carga
this.st_1=create st_1
this.uo_selespecie=create uo_selespecie
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_5
this.Control[iCurrent+3]=this.uo_selcliente
this.Control[iCurrent+4]=this.uo_selzonas
this.Control[iCurrent+5]=this.cb_carga
this.Control[iCurrent+6]=this.dw_carga
this.Control[iCurrent+7]=this.st_1
this.Control[iCurrent+8]=this.uo_selespecie
end on

on w_mant_mues_pmg_exportadora.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_5)
destroy(this.uo_selcliente)
destroy(this.uo_selzonas)
destroy(this.cb_carga)
destroy(this.dw_carga)
destroy(this.st_1)
destroy(this.uo_selespecie)
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
istr_Mant.Argumento[4] = String(uo_SelEspecie.Codigo)

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
If IsNull(uo_SelEspecie.Codigo) Then lb_Cerrar = True

If lb_Cerrar Then
	Close(This)
Else
	iuo_Semana	=	Create uo_semanafactura	
	
	uo_SelCliente.Seleccion(False, False)
	uo_SelZonas.Seleccion(False, False)
	uo_SelEspecie.Seleccion(False, False)
	
	uo_SelCliente.Inicia(gi_CodExport)
	
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
	istr_Mant.Argumento[4] = String(uo_SelEspecie.Codigo)
	
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
integer y = 468
integer width = 3127
integer height = 1332
integer taborder = 50
string dataobject = "dw_mues_pmg_exportadora"
end type

type st_encabe from w_mant_tabla`st_encabe within w_mant_mues_pmg_exportadora
integer width = 3127
integer height = 380
end type

type pb_lectura from w_mant_tabla`pb_lectura within w_mant_mues_pmg_exportadora
integer x = 3342
integer y = 52
integer taborder = 40
end type

event pb_lectura::clicked;call super::clicked;uo_SelCliente.Bloquear(True)
uo_SelZonas.Bloquear(True)
end event

type pb_nuevo from w_mant_tabla`pb_nuevo within w_mant_mues_pmg_exportadora
integer x = 3342
integer y = 508
integer taborder = 60
end type

event pb_nuevo::clicked;call super::clicked;uo_SelCliente.Bloquear(False)
uo_SelZonas.Bloquear(False)
cb_carga.Enabled	= False

end event

type pb_insertar from w_mant_tabla`pb_insertar within w_mant_mues_pmg_exportadora
integer x = 3342
integer y = 684
integer taborder = 70
end type

type pb_eliminar from w_mant_tabla`pb_eliminar within w_mant_mues_pmg_exportadora
integer x = 3342
integer y = 860
integer taborder = 80
end type

type pb_grabar from w_mant_tabla`pb_grabar within w_mant_mues_pmg_exportadora
integer x = 3342
integer y = 1036
integer taborder = 90
end type

type pb_imprimir from w_mant_tabla`pb_imprimir within w_mant_mues_pmg_exportadora
integer x = 3342
integer y = 1212
integer taborder = 100
end type

type pb_salir from w_mant_tabla`pb_salir within w_mant_mues_pmg_exportadora
integer x = 3342
integer y = 1516
integer taborder = 110
end type

type st_2 from statictext within w_mant_mues_pmg_exportadora
integer x = 1563
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
integer x = 1897
integer y = 148
integer height = 92
integer taborder = 50
boolean bringtotop = true
end type

on uo_selzonas.destroy
call uo_seleccion_zonas::destroy
end on

type cb_carga from commandbutton within w_mant_mues_pmg_exportadora
integer x = 2203
integer y = 300
integer width = 590
integer height = 116
integer taborder = 60
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
boolean enabled = false
string text = "Carga Inicial"
boolean default = true
end type

event clicked;
If dw_1.RowCount() > 0 Then
	If MessageBox('Alerta', 'Existe Informacion cargada. Desea Borrar', Exclamation!, YesNo!, 2) = 2 Then 
		Return 
	Else
		dw_1.RowsMove(1, dw_1.RowCount(), Primary!, dw_1, 1, Delete!)
		Parent.TriggerEvent("ue_guardar")
		wf_CargaArchivo()
	End if
Else
	wf_CargaArchivo()
End If
end event

type dw_carga from datawindow within w_mant_mues_pmg_exportadora
boolean visible = false
integer x = 2601
integer y = 1412
integer width = 594
integer height = 336
integer taborder = 100
boolean bringtotop = true
string title = "none"
string dataobject = "dw_carga_estandar"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

type st_1 from statictext within w_mant_mues_pmg_exportadora
integer x = 187
integer y = 300
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
string text = "Especie"
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_mant_mues_pmg_exportadora
integer x = 567
integer y = 288
integer height = 92
integer taborder = 40
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

