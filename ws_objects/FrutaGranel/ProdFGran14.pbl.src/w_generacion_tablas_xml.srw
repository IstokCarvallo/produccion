$PBExportHeader$w_generacion_tablas_xml.srw
$PBExportComments$Generacion de Archivos XML
forward
global type w_generacion_tablas_xml from w_para_informes
end type
type st_4 from statictext within w_generacion_tablas_xml
end type
type dw_1 from uo_dw within w_generacion_tablas_xml
end type
type sle_mensaje from singlelineedit within w_generacion_tablas_xml
end type
type dw_2 from uo_dw within w_generacion_tablas_xml
end type
type dw_3 from uo_dw within w_generacion_tablas_xml
end type
type uo_selespecie from uo_seleccion_especie within w_generacion_tablas_xml
end type
type ole_zip from olecustomcontrol within w_generacion_tablas_xml
end type
type st_1 from statictext within w_generacion_tablas_xml
end type
end forward

global type w_generacion_tablas_xml from w_para_informes
string tag = "Generacion de Archivos XML"
integer x = 14
integer y = 32
integer width = 2610
integer height = 1240
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "AppIcon!"
boolean clientedge = true
st_4 st_4
dw_1 dw_1
sle_mensaje sle_mensaje
dw_2 dw_2
dw_3 dw_3
uo_selespecie uo_selespecie
ole_zip ole_zip
st_1 st_1
end type
global w_generacion_tablas_xml w_generacion_tablas_xml

type variables
String		is_Ruta , is_Tablas[]
Integer	ii_Tipo
end variables

forward prototypes
public subroutine wf_enviamail ()
public subroutine wf_envio ()
public subroutine wf_recepcion ()
public function boolean wf_comprime (string archivo, integer tipo)
public subroutine wf_cargaobjeto (datawindow adw, string objeto, integer tipo)
public function boolean wf_procesa (string archivo, integer tipo)
public function long wf_busca (datawindow adw, datawindow fuente, long fila, integer tipo)
public function boolean wf_graba (datawindow adw, boolean borrando)
public subroutine wf_actualiza (datawindow target, datawindow source, long row, long fila)
end prototypes

public subroutine wf_enviamail ();Long	ll_Result, ll_Fila
String	ls_ErrorMsg, ls_Adjuntos, ls_Archivo, ls_Para

dw_2.Reset()
dw_2.Retrieve()

For ll_Fila = 1 To UpperBound(is_Tablas)
	ls_Archivo = Mid(is_Tablas[ll_Fila], LastPos(is_Tablas[ll_Fila], '_') + 1, Len(is_Tablas[ll_Fila]))
	If ll_Fila = UpperBound(is_Tablas) Then
		ls_Adjuntos += is_Ruta + '\ArchivosXML\' + ls_Archivo + '.gzp'
	Else
		ls_Adjuntos += is_Ruta + '\ArchivosXML\' + ls_Archivo + '.gzp;'
	End If
Next

For ll_Fila = 1 To dw_2.RowCount()
	If ll_Fila = dw_2.RowCount() Then
		ls_para += '<' + dw_2.Object.cxml_correo[ll_Fila] + '>'
	Else
		ls_para += '<' + dw_2.Object.cxml_correo[ll_Fila] + '>;'
	End If
Next

//ll_Result	= send_mail('smtp.rioblanco.cl', '<granel@rioblanco.cl>', ls_Para,'','', 'Actualizacion de Tablas.', 'Favor Actualizar Informacion Correspondiente.', ls_Adjuntos, ls_ErrorMsg)
If (ll_Result < 0) Then
	MessageBox("Error No" + string(ll_Result), ls_ErrorMsg)
End If
end subroutine

public subroutine wf_envio ();Long	ll_Fila
String	ls_Archivo
	
If Not DirectoryExists(is_Ruta + '\ArchivosXML') Then
	CreateDirectory (is_Ruta + '\ArchivosXML')
End If

For ll_Fila = 1 To UpperBound(is_Tablas)
	wf_CargaObjeto(dw_1, is_Tablas[ll_Fila], 1)
	
	ls_Archivo = Mid(is_Tablas[ll_Fila], LastPos(is_Tablas[ll_Fila], '_') + 1, Len(is_Tablas[ll_Fila]))
	sle_mensaje.Text = 'Procesando Archivo: ' + ls_Archivo
	
	If dw_1.SaveAs(is_Ruta + '\ArchivosXML\' + ls_Archivo + '.xml', XML!, True) = 1 Then 
//		If wf_comprime(ls_Archivo, 1) Then
//			FileDelete(is_Ruta + '\ArchivosXML\' + ls_Archivo + '.xml')
//			sle_Mensaje.Text = 'Eliminando Archivo: ' + ls_Archivo + '.xml'
//		End If
	End If
Next

sle_Mensaje.Text = 'Enviando Correo:'

//wf_EnviaMail()
end subroutine

public subroutine wf_recepcion ();Long	ll_Fila
String	ls_Archivo
	
If Not DirectoryExists(is_Ruta + '\ArchivosXML') Then
	MessageBox('Error', 'No se Encuentra directorio de Trabajo \ArchivosXML', StopSign!, OK!)
	Return
End If

For ll_Fila = 1 To UpperBound(is_Tablas)
	dw_1.Reset()
	dw_3.Reset()
	wf_CargaObjeto(dw_1, is_Tablas[ll_Fila], 1)
	wf_CargaObjeto(dw_3, is_Tablas[ll_Fila], 0)
	
	ls_Archivo = Mid(is_Tablas[ll_Fila], LastPos(is_Tablas[ll_Fila], '_') + 1, Len(is_Tablas[ll_Fila]))
	
//	If FileExists(is_Ruta + '\ArchivosXML\' + ls_Archivo + '.gzp') Then
//		If wf_comprime(ls_Archivo, 2) Then
//			if ll_fila = 17 then
//				sle_Mensaje.Text = 'Aca'
//			end if
			If wf_procesa(ls_Archivo, ll_Fila) Then
				sle_Mensaje.Text = 'Grabando Archivo: ' + ls_Archivo + '.gzp'
				wf_Graba(dw_1, False)
				FileDelete(is_Ruta + '\ArchivosXML\' + ls_Archivo + '.gzp')
				sle_Mensaje.Text = 'Eliminando Archivo: ' + ls_Archivo + '.gzp'
			Else
				sle_Mensaje.Text = 'Archivo: ' + ls_Archivo + '.gzp No se Proceso.'
			End if
//		End If
//	End If
Next

For ll_Fila = 1 To UpperBound(is_Tablas)
	ls_Archivo = Mid(is_Tablas[ll_Fila], LastPos(is_Tablas[ll_Fila], '_') + 1, Len(is_Tablas[ll_Fila]))
	FileDelete(is_Ruta + '\ArchivosXML\' + ls_Archivo + '.xml')
Next
end subroutine

public function boolean wf_comprime (string archivo, integer tipo);Boolean	lb_Retorno = True
Long		ll_Fila
String		ls_Archivo

sle_mensaje.Text = 'Comprimiendo: ' + Archivo 

ole_zip.object.level		= 9
If Tipo = 1 Then
	sle_mensaje.Text = 'Comprimiendo: ' + Archivo 
	ole_zip.object.inputfile	= is_Ruta + '\ArchivosXML\' + Archivo + '.xml'
	ole_zip.object.outputfile	= is_Ruta + '\ArchivosXML\' + Archivo + '.gzp'
	ole_zip.object.compress()
Else
	sle_mensaje.Text = 'Descomprimiendo: ' + Archivo 
	ole_zip.object.inputfile	= is_Ruta + '\ArchivosXML\' + Archivo + '.gzp'
	ole_zip.object.outputfile	= is_Ruta + '\ArchivosXML\' + Archivo + '.xml'
	ole_zip.object.Decompress()
End If
		
Return lb_Retorno
end function

public subroutine wf_cargaobjeto (datawindow adw, string objeto, integer tipo);adw.DataObject = Objeto
adw.SetTransObject(Sqlca)

IF Tipo = 1 THEN 
	adw.Retrieve(uo_SelEspecie.Codigo)
END IF

Return
end subroutine

public function boolean wf_procesa (string archivo, integer tipo);Boolean	lb_Retorno = True
Long		ll_Fila, ll_Busca, ll_New

dw_3.ImportFile(XML!, is_Ruta + '\ArchivosXML\' + Archivo + '.xml')
sle_Mensaje.Text = 'Procesando Archivo: ' + Archivo + '.xml'

If dw_3.RowCount() = 0 Then 
	lb_Retorno = False
Else
	For ll_Fila = 1 To dw_3.RowCount()
		sle_mensaje.Text = 'Procesando Archivo: ' + Archivo + ' (' + String(ll_Fila, '#,##0') + ' de ' +  String(dw_3.RowCount(), '#,##0') + ')'
		
		ll_Busca = wf_Busca(dw_1, dw_3, ll_Fila, Tipo)
		
		If ll_Busca = 0 Then
			dw_3.RowsCopy(ll_Fila, ll_Fila, Primary!, dw_1, dw_1.RowCount() + 1, Primary!)
		Else
			wf_Actualiza(dw_1, dw_3, ll_Fila, ll_Busca)
		End If
	Next		
End If
	
Return lb_Retorno
end function

public function long wf_busca (datawindow adw, datawindow fuente, long fila, integer tipo);Long	ll_Retorno = -1
String	ls_Busqueda

Choose Case Tipo
	Case 1
		ls_Busqueda = 'enva_tipoen = ' + String(Fuente.Object.enva_tipoen[Fila])
		
	Case 2
		ls_Busqueda = 'enva_tipoen = ' + String(Fuente.Object.enva_tipoen[Fila]) + ' And enva_codigo = ' + String(Fuente.Object.enva_codigo[Fila])
		
	Case 3
		ls_Busqueda = 'Upper(emba_codigo) = "' + Upper(Fuente.Object.emba_codigo[Fila]) + '" And clie_codigo = ' + String(Fuente.Object.clie_codigo[Fila])
		
	Case 4
		ls_Busqueda = 'enva_tipoen = ' + String(Fuente.Object.enva_tipoen[Fila]) + ' And enva_codigo = ' + String(Fuente.Object.enva_codigo[Fila]) + &
							' And cale_calida = "' + Upper(Fuente.Object.cale_calida[Fila]) + '"'
							
	Case 5
		ls_Busqueda = 'espe_codigo = ' + String(Fuente.Object.espe_codigo[Fila])
		
	Case 6
		ls_Busqueda = 'espe_codigo = ' + String(Fuente.Object.espe_codigo[Fila]) + ' And vari_codigo = ' + String(Fuente.Object.vari_codigo[Fila])
		
	Case 7
		ls_Busqueda = 'clie_codigo = ' + String(Fuente.Object.clie_codigo[Fila]) + ' And '  + &
						  'upper(emba_codigo) = "' + Upper(Fuente.Object.emba_codigo[Fila]) + '" And ' + &
						  'upper(tpem_codigo) = "' + Upper(Fuente.Object.tpem_codigo[Fila]) + '"'
		
	Case 8
		ls_Busqueda = 'espe_codigo = ' + String(Fuente.Object.espe_codigo[Fila]) + ' And ' + &
						  'enva_tipoen = ' + String(Fuente.Object.enva_tipoen[Fila]) + ' And ' + &
						  'enva_codigo = ' + String(Fuente.Object.enva_codigo[Fila]) + ' And ' + &
						  'UPPER(caen_calibr) = "' + Upper(Fuente.Object.caen_calibr[Fila]) + '"'
		
	Case 9
		ls_Busqueda = 'espe_codigo = ' + String(Fuente.Object.espe_codigo[Fila]) + ' And vari_codigo = ' + String(Fuente.Object.vari_codigo[Fila]) +&
							' And vaca_calibr = "' + Upper(Fuente.Object.vaca_calibr[Fila]) + '"'
	Case 10
		ls_Busqueda = 'tipr_Codigo = ' + String(Fuente.Object.tipr_Codigo[Fila])	
		
	Case 11
		ls_Busqueda = 'zona_codigo = ' + String(Fuente.Object.zona_codigo[Fila])			
		
	Case 12
		ls_Busqueda = 'prod_codigo = ' + String(Fuente.Object.prod_codigo[Fila])
			
	Case 13
		ls_Busqueda = 'cate_codigo = ' + String(Fuente.Object.cate_codigo[Fila])
		
	Case 14
		ls_Busqueda = 'etiq_codigo = ' + String(Fuente.Object.etiq_codigo[Fila])
		
	Case 15
		ls_Busqueda = 'envo_codigo = ' + String(Fuente.Object.envo_codigo[Fila])
		
	Case 16
		ls_Busqueda = 'clie_codigo = ' + String(Fuente.Object.clie_codigo[Fila]) + ' And ' + &
						  'espe_codigo = ' + String(Fuente.Object.espe_codigo[Fila]) + ' And ' + &
						  'vari_codigo = ' + String(Fuente.Object.vari_codigo[Fila]) + ' And ' + &
						  'Upper(emba_codigo) = "' + Upper(Fuente.Object.emba_codigo[Fila]) + '" And ' + &
						  'gtin_calibr = "' + Upper(Fuente.Object.gtin_calibr[Fila]) + '"'
						
	Case 17
		ls_Busqueda = 'plde_codigo = ' + String(Fuente.Object.plde_codigo[Fila])
		
	Case 18
		ls_Busqueda = 'prod_codigo = ' + String(Fuente.Object.prod_codigo[Fila]) + ' And prpr_codigo = ' + String(Fuente.Object.prpr_codigo[Fila])
		
	Case 19
		ls_Busqueda = 'prod_codigo = ' + String(Fuente.Object.prod_codigo[Fila]) + ' And prpr_codigo = ' + String(Fuente.Object.prpr_codigo[Fila]) + &
						' And prcc_codigo = ' + String(Fuente.Object.prcc_codigo[Fila])
			
		

End Choose

ll_Retorno = adw.Find(ls_Busqueda, 1, adw.RowCount())

Return ll_Retorno
end function

public function boolean wf_graba (datawindow adw, boolean borrando);Boolean	lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

If Borrando Then
	If dw_1.Update(True, False) = 1 Then
		Commit;
		
		If sqlca.SQLCode <> 0 Then
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		Else
			lb_Retorno	=	True
			
			dw_1.ResetUpdate()
		End If
	Else
		F_ErrorBaseDatos(sqlca, This.Title)
	End If
Else
	If dw_1.Update(True, False) = 1 Then
		Commit;
		
		If sqlca.SQLCode <> 0 Then
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		Else
			lb_Retorno	=	True
			
			dw_1.ResetUpdate()
		End If
	Else
		F_ErrorBaseDatos(sqlca, This.Title)
		
		RollBack;
	End If
End If

sqlca.AutoCommit	=	lb_AutoCommit

Return lb_Retorno
end function

public subroutine wf_actualiza (datawindow target, datawindow source, long row, long fila);String	ls_Columna, ls_Tipo
Long		ll_Fila, ll_Control

ll_Fila = 1 
ll_Control	= Source.SetColumn(ll_Fila)

Do While ll_Control <> -1
	ls_Columna	= Source.GetColumnName()
	ls_Tipo		= Lower(Mid(Source.Describe(Source.GetColumnName()+ '.ColType') , 1, Pos(Source.Describe(Source.GetColumnName()+ '.ColType'), '(') - 1))
	
	Choose Case ls_Tipo
		Case 'decimal'
			If Not IsNull(Source.GetItemDecimal(Row, ls_Columna)) Then 
				If Source.GetItemDecimal(Row, ls_Columna) <> Target.GetItemDecimal(Fila, ls_Columna) Then
					Target.SetItem(Fila, ls_Columna, Source.GetItemDecimal(Row, ls_Columna))
				End If
			End If
			
		Case 'number'
			If Not IsNull(Source.GetItemNumber(Row, ls_Columna)) Then 
				If Source.GetItemNumber(Row, ls_Columna) <> Target.GetItemNumber(Fila, ls_Columna) Then
					Target.SetItem(Fila, ls_Columna, Source.GetItemNumber(Row, ls_Columna))
				End If
			End If
			
		Case 'date'
			If Not IsNull(Source.GetItemDate(Row, ls_Columna)) Then 
				If Source.GetItemDate(Row, ls_Columna) <> Target.GetItemDate(Fila, ls_Columna) Then
					Target.SetItem(Fila, ls_Columna, Source.GetItemDate(Row, ls_Columna))
				End If
			End If

		Case 'time'
			If Not IsNull(Source.GetItemTime(Row, ls_Columna)) Then 
				If Source.GetItemTime(Row, ls_Columna) <> Target.GetItemTime(Fila, ls_Columna) Then
					Target.SetItem(Fila, ls_Columna, Source.GetItemTime(Row, ls_Columna))
				End If 
			End If
			
		Case 'char'
			If Not IsNull(Source.GetItemString(Row, ls_Columna)) Then 
				If Upper(Source.GetItemString(Row, ls_Columna)) <> Upper(Target.GetItemString(Fila, ls_Columna)) Then
					Target.SetItem(Fila, ls_Columna, Source.GetItemString(Row, ls_Columna))
				End If
			End If
			
	End Choose
	ll_Fila++
	ll_Control	= Source.SetColumn(ll_Fila)
Loop

Return
end subroutine

on w_generacion_tablas_xml.create
int iCurrent
call super::create
this.st_4=create st_4
this.dw_1=create dw_1
this.sle_mensaje=create sle_mensaje
this.dw_2=create dw_2
this.dw_3=create dw_3
this.uo_selespecie=create uo_selespecie
this.ole_zip=create ole_zip
this.st_1=create st_1
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.dw_1
this.Control[iCurrent+3]=this.sle_mensaje
this.Control[iCurrent+4]=this.dw_2
this.Control[iCurrent+5]=this.dw_3
this.Control[iCurrent+6]=this.uo_selespecie
this.Control[iCurrent+7]=this.ole_zip
this.Control[iCurrent+8]=this.st_1
end on

on w_generacion_tablas_xml.destroy
call super::destroy
destroy(this.st_4)
destroy(this.dw_1)
destroy(this.sle_mensaje)
destroy(this.dw_2)
destroy(this.dw_3)
destroy(this.uo_selespecie)
destroy(this.ole_zip)
destroy(this.st_1)
end on

event open;call super::open;RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, is_Ruta)

is_Tablas[01] 	=	'dw_gene_tiposenvases'
is_Tablas[02] 	= 	'dw_gene_envases'
is_Tablas[03] 	= 	'dw_gene_embalajesprod'
is_Tablas[04] 	= 	'dw_gene_calicosechero'
//is_Tablas[05] 	= 	'dw_gene_especies'
//is_Tablas[06] 	= 	'dw_gene_variedades'
is_Tablas[07] 	= 	'dw_gene_tipopallemba'
is_Tablas[08] 	= 	'dw_gene_calibresenvase'
is_Tablas[09]	= 	'dw_gene_varicalibres'
//is_Tablas[10]	= 	'dw_gene_tiporpductor'
//is_Tablas[11]	= 	'dw_gene_zonas'
//is_Tablas[12]	= 	'dw_gene_productores'
//is_Tablas[13]	= 	'dw_gene_categorias'
//is_Tablas[14]	= 	'dw_gene_etiquetas'
//is_Tablas[15]	= 	'dw_gene_envoltorios'
is_Tablas[16]	= 	'dw_gene_gtin14'
//is_Tablas[17]	= 	'dw_gene_plantas'
//is_Tablas[18]	= 	'dw_gene_prodpredios'
//is_Tablas[19]	= 	'dw_gene_prodcuarteles'
//
dw_2.SetTransObject(Sqlca)

ii_Tipo	=	Integer(Message.StringParm)

If ii_Tipo = 1 Then
	st_titulo.Text = 'Generacion de Archivos XML'
Else
	st_titulo.Text = 'Lectura de Archivos XML'
	st_1.Visible = False
	uo_SelEspecie.Visible = False
End If

uo_SelEspecie.Seleccion(False, False)
end event

type pb_excel from w_para_informes`pb_excel within w_generacion_tablas_xml
end type

type st_computador from w_para_informes`st_computador within w_generacion_tablas_xml
end type

type st_usuario from w_para_informes`st_usuario within w_generacion_tablas_xml
end type

type st_temporada from w_para_informes`st_temporada within w_generacion_tablas_xml
end type

type p_logo from w_para_informes`p_logo within w_generacion_tablas_xml
end type

type st_titulo from w_para_informes`st_titulo within w_generacion_tablas_xml
integer width = 1751
end type

type pb_acepta from w_para_informes`pb_acepta within w_generacion_tablas_xml
string tag = "Generacion de Archivos XML"
integer x = 2121
integer y = 416
integer taborder = 30
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
string powertiptext = "Generacion de Archivos XML"
end type

event pb_acepta::clicked;Long		ll_Fila, ll_Start, ll_Used
String	ls_Archivo

SetPointer(HourGlass!)
pb_Acepta.Enabled = False

If ii_Tipo = 1 Then
	If IsNull(uo_SelEspecie.Codigo) Or uo_SelEspecie.Codigo = -1 Then
		MessageBox('Atencion', 'Debe ingresar una especie.')
		pb_Acepta.Enabled = True
		Return
	End If
End If

ll_start = Cpu()

If ii_Tipo = 1 Then
	wf_envio()
Else
	wf_Recepcion()
End If

ll_used = Cpu() - ll_start

sle_mensaje.Text = 'Tiempo de Proceso: ' + String (ll_Used / 1000, '##0.00') + ' Segundos.'

pb_Acepta.Enabled = True
SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_generacion_tablas_xml
integer x = 2121
integer y = 692
integer taborder = 40
string powertiptext = "Salir de la Ventana"
end type

type st_4 from statictext within w_generacion_tablas_xml
integer x = 247
integer y = 432
integer width = 1751
integer height = 516
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type dw_1 from uo_dw within w_generacion_tablas_xml
boolean visible = false
integer x = 37
integer y = 656
integer width = 169
integer height = 116
integer taborder = 11
boolean bringtotop = true
boolean vscrollbar = false
boolean resizable = true
end type

type sle_mensaje from singlelineedit within w_generacion_tablas_xml
integer x = 302
integer y = 672
integer width = 1650
integer height = 88
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
borderstyle borderstyle = stylelowered!
end type

type dw_2 from uo_dw within w_generacion_tablas_xml
boolean visible = false
integer x = 37
integer y = 508
integer width = 169
integer height = 116
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mues_correosxml"
boolean vscrollbar = false
end type

type dw_3 from uo_dw within w_generacion_tablas_xml
boolean visible = false
integer x = 37
integer y = 804
integer width = 169
integer height = 116
integer taborder = 11
boolean bringtotop = true
boolean vscrollbar = false
end type

type uo_selespecie from uo_seleccion_especie within w_generacion_tablas_xml
integer x = 923
integer y = 496
integer height = 88
integer taborder = 40
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type ole_zip from olecustomcontrol within w_generacion_tablas_xml
event progress ( integer percent_complete )
integer x = 302
integer y = 812
integer width = 1650
integer height = 84
integer taborder = 60
boolean bringtotop = true
borderstyle borderstyle = stylelowered!
long backcolor = 30586022
boolean focusrectangle = false
string binarykey = "w_generacion_tablas_xml.win"
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 8388608
end type

type st_1 from statictext within w_generacion_tablas_xml
integer x = 379
integer y = 508
integer width = 512
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
string text = "Especie Embalaje"
boolean focusrectangle = false
end type


Start of PowerBuilder Binary Data Section : Do NOT Edit
0Ew_generacion_tablas_xml.bin 
2B00000600e011cfd0e11ab1a1000000000000000000000000000000000003003e0009fffe00000006000000000000000000000001000000010000000000001000fffffffe00000000fffffffe0000000000000000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffdfffffffeffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff006f00520074006f004500200074006e00790072000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000050016ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000fffffffe00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffffffffffffffffffff000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1Ew_generacion_tablas_xml.bin 
End of PowerBuilder Binary Data Section : No Source Expected After This Point
