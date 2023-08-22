$PBExportHeader$w_carga_temperaturas.srw
forward
global type w_carga_temperaturas from window
end type
type dw_2 from uo_dw within w_carga_temperaturas
end type
type plb_1 from picturelistbox within w_carga_temperaturas
end type
type st_1 from statictext within w_carga_temperaturas
end type
type uo_selplanta from uo_seleccion_plantas within w_carga_temperaturas
end type
type pb_imprimir from picturebutton within w_carga_temperaturas
end type
type pb_lectura from picturebutton within w_carga_temperaturas
end type
type dw_1 from uo_dw within w_carga_temperaturas
end type
type pb_salir from picturebutton within w_carga_temperaturas
end type
type pb_nuevo from picturebutton within w_carga_temperaturas
end type
type st_encabe from statictext within w_carga_temperaturas
end type
type plb_documentos from picturelistbox within w_carga_temperaturas
end type
type dw_3 from uo_dw within w_carga_temperaturas
end type
end forward

global type w_carga_temperaturas from window
integer x = 5
integer y = 16
integer width = 4037
integer height = 2084
boolean titlebar = true
string title = "Carga Archivos Frigorifico - ATS"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 16777215
event ue_asignacion ( )
dw_2 dw_2
plb_1 plb_1
st_1 st_1
uo_selplanta uo_selplanta
pb_imprimir pb_imprimir
pb_lectura pb_lectura
dw_1 dw_1
pb_salir pb_salir
pb_nuevo pb_nuevo
st_encabe st_encabe
plb_documentos plb_documentos
dw_3 dw_3
end type
global w_carga_temperaturas w_carga_temperaturas

type variables
CONSTANT	Integer	MaximoImagenes = 15

Long 		il_fila
Date		id_FechaAcceso
Time		it_HoraAcceso
String		is_Ruta, is_Filtro

DataWindowChild	idwc_Camara
uo_CamarasBode	iuo_Camara

Menu		im_menu

Str_parms		istr_parms
Str_mant			istr_mant
Str_busqueda	istr_busq
Str_info			istr_info
end variables

forward prototypes
public function integer wf_inicializa ()
public function boolean wf_carga_documentos (string as_tipo)
public function boolean wf_duplicado (string as_ruta, string as_archivo)
public subroutine wf_carga_archivos ()
public function integer wf_mueve_archivos (string as_archivo)
public function boolean wf_cargatablas ()
protected function boolean wf_actualiza_db (boolean borrando)
end prototypes

public function integer wf_inicializa ();Integer	li_Imagen

plb_1.Reset()
plb_documentos.Reset()
plb_documentos.PictureHeight	=	20
plb_documentos.PictureWidth	=	20
li_Imagen	=	plb_documentos.AddPicture("\Desarrollo 17\Imagenes\Botones\Excel.png")

Return	li_Imagen
end function

public function boolean wf_carga_documentos (string as_tipo);Boolean	lb_Retorno = True
Integer	li_Imagen, li_Fila

li_Imagen	=	wf_inicializa()

If plb_1.DirList(is_Ruta + '\' + as_Tipo, 32) Then
	For li_Fila = 1 To plb_1.TotalItems()
		plb_documentos.AddItem(plb_1.Text(li_Fila), li_Imagen)
	Next
Else
	MessageBox('Alerta', 'No se pudo cargar informacion en ruta seleccionada', Information!, Ok!)
	lb_Retorno = False
End If

Return	lb_Retorno
end function

public function boolean wf_duplicado (string as_ruta, string as_archivo);Boolean	lb_Retorno = True
Long		ll_Busca

Choose case istr_mant.Respuesta
	Case 1
		ll_Busca = dw_1.Find('reid_ruta = "' + as_Ruta + '" And reid_archiv = "' + as_Archivo + '"', 1, dw_1.RowCount())
	Case 2
		ll_Busca = dw_1.Find('repd_ruta = "' + as_Ruta + '" And repd_archiv = "' + as_Archivo + '"', 1, dw_1.RowCount())
End Choose

If ll_Busca > 0 Then Return lb_Retorno = False

Return lb_Retorno
end function

public subroutine wf_carga_archivos ();Long	ll_Fila, ll_Carga

dw_2.Reset()

For ll_Fila = 1 To plb_Documentos.TotalItems()
	If plb_Documentos.State(ll_Fila) = 1 Then
		ll_Carga = dw_2.ImportFile(CSV!, is_Ruta + "\" + plb_Documentos.Text(ll_Fila))
		
		If ll_Carga > 0 Then
			If wf_CargaTablas() Then
				If wf_actualiza_db(False) Then
					wf_Mueve_Archivos(plb_Documentos.Text(ll_Fila))
					wf_carga_documentos(is_Filtro)
					w_main.SetMicroHelp("Información Grabada.")
				Else
					MessageBox('Error...', "No se puede Grabar información.")
					Return
				End If
			Else
				MessageBox('Error...', "No se pudo cargar el Archivo " +  plb_Documentos.Text(ll_Fila) + " , posiblemente ya fue cargado a Base de Datos.")
				Return
			End If
		Else
			MessageBox('Error...', "No se pudo cargar el Archivo: " +  plb_Documentos.Text(ll_Fila))
			Return
		End If		
	End If
Next
end subroutine

public function integer wf_mueve_archivos (string as_archivo);Integer	li_Retorno
String		ls_Source, ls_Target

ls_Target	= is_Ruta + "\" + "ATS_Movidos"
ls_Source	= GetCurrentDirectory()


If FileExists(as_Archivo) Then 
	If Not DirectoryExists(ls_Target) Then CreateDirectory (ls_Target)
	
	If FileMove(ls_Source + "\" + as_Archivo, ls_Target + "\" + as_Archivo) > 0 Then 
		li_Retorno = 0
	Else
		li_Retorno = -2
	End If
Else
	li_Retorno = -1
End If

Return li_Retorno
end function

public function boolean wf_cargatablas ();Boolean	lb_Retorno = True
String		ls_Find, ls_Folio, ls_Cliente
Long		ll_Fila, ll_New, ll_NewD, ll_Find	, ll_Planta, ll_Camara, ll_Numero

dw_3.Reset()

For ll_Fila = 1 To dw_2.RowCount()
	
	iuo_Camara.ExisteATS(dw_2.Object.plde_codigo[ll_Fila], dw_2.Object.cama_codigo[ll_Fila], False, Sqlca)
	
	ll_Planta		=	dw_2.Object.plde_codigo[ll_Fila]
	ll_Camara	=	iuo_Camara.Camara
	ll_Numero	=	dw_2.Object.teen_numero[ll_Fila]
	
	ls_Find = 'plde_codigo = ' + String(ll_Planta) + ' And cama_codigo = ' + String(ll_Camara) + ' And teen_numero = ' + String(ll_Numero)	
	ll_Find = dw_1.Find(ls_Find, 1, dw_1.RowCount())
	
	If ll_Find = 0 Then	
		ll_New = dw_1.InsertRow(0)

		dw_1.Object.plde_codigo[ll_New]		= ll_Planta
		dw_1.Object.cama_codigo[ll_New]	= ll_Camara
		dw_1.Object.teen_numero[ll_New]	= ll_Numero
		dw_1.Object.teen_fechas[ll_New]		= dw_2.Object.teen_fecini[ll_Fila]
		dw_1.Object.teen_fecini[ll_New]		= dw_2.Object.teen_fecini[ll_Fila]
		dw_1.Object.teen_fecfin[ll_New]		= dw_2.Object.teen_fecter[ll_Fila]
//		dw_1.Object.teen_fecvol[ll_New]		= 
		dw_1.Object.teen_protem[ll_New]		= Dec(F_Global_Replace(dw_2.Object.teen_temfin[ll_Fila], '.', ','))
	End If
		ll_NewD = dw_3.InsertRow(0)
		
		If LenA(Trim(dw_2.Object.paen_numero[ll_Fila])) = 10 Then
			ls_Folio		= Right(dw_2.Object.paen_numero[ll_Fila], 7)
			ls_Cliente	= Mid(dw_2.Object.paen_numero[ll_Fila], 1, 3)
		Else
			ls_Folio		= dw_2.Object.paen_numero[ll_Fila]
			ls_Cliente	= ""
		End If
	
		dw_3.Object.plde_codigo[ll_NewD]	= ll_Planta
		dw_3.Object.cama_codigo[ll_NewD]	= ll_Camara
		dw_3.Object.teen_numero[ll_NewD]	= ll_Numero
		dw_3.Object.paen_numero[ll_NewD]	= Long(ls_Folio)
		dw_3.Object.clie_rotula[ll_NewD]		= ls_Cliente
		dw_3.Object.tede_pincha[ll_NewD]	= dw_2.Object.tede_pincha[ll_Fila]
		dw_3.Object.tede_fecmar[ll_NewD]	= dw_2.Object.tede_fecmar[ll_Fila]
		dw_3.Object.tede_nrosen[ll_NewD]	= dw_2.Object.tede_nrosen[ll_Fila]
		dw_3.Object.tede_temper[ll_NewD]	= Dec(F_Global_Replace(dw_2.Object.tede_temper[ll_Fila], '.', ','))
		dw_3.Object.tede_tipsen[ll_NewD]	= dw_2.Object.tede_tipsen[ll_Fila]
	
//	Else
//		lb_Retorno = False
//		Exit
//	End If 
Next

Return lb_Retorno
end function

protected function boolean wf_actualiza_db (boolean borrando);Boolean	lb_AutoCommit, lb_Retorno
Long numero
Integer li_planta, li_movto

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

If Borrando Then
	If dw_3.Update(True, False) = 1 Then
		If dw_1.Update(True, False) = 1 Then
			Commit;
			
			If sqlca.SQLCode <> 0 Then
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			Else
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_3.ResetUpdate()
			End If
		Else
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		End If
	Else
		F_ErrorBaseDatos(sqlca, This.Title)
	End If
Else
	If dw_1.Update(True, False) = 1 Then
		If dw_3.Update(True, False) = 1 Then
			Commit;
			
			If sqlca.SQLCode <> 0 Then
				F_ErrorBaseDatos(sqlca, This.Title)
				
				RollBack;
			Else
				lb_Retorno	=	True
				
				dw_1.ResetUpdate()
				dw_3.ResetUpdate()
			End If
		Else
			F_ErrorBaseDatos(sqlca, This.Title)
			
			RollBack;
		End If
	Else
		F_ErrorBaseDatos(sqlca, This.Title)
	End If
End If
 
sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

on w_carga_temperaturas.create
this.dw_2=create dw_2
this.plb_1=create plb_1
this.st_1=create st_1
this.uo_selplanta=create uo_selplanta
this.pb_imprimir=create pb_imprimir
this.pb_lectura=create pb_lectura
this.dw_1=create dw_1
this.pb_salir=create pb_salir
this.pb_nuevo=create pb_nuevo
this.st_encabe=create st_encabe
this.plb_documentos=create plb_documentos
this.dw_3=create dw_3
this.Control[]={this.dw_2,&
this.plb_1,&
this.st_1,&
this.uo_selplanta,&
this.pb_imprimir,&
this.pb_lectura,&
this.dw_1,&
this.pb_salir,&
this.pb_nuevo,&
this.st_encabe,&
this.plb_documentos,&
this.dw_3}
end on

on w_carga_temperaturas.destroy
destroy(this.dw_2)
destroy(this.plb_1)
destroy(this.st_1)
destroy(this.uo_selplanta)
destroy(this.pb_imprimir)
destroy(this.pb_lectura)
destroy(this.dw_1)
destroy(this.pb_salir)
destroy(this.pb_nuevo)
destroy(this.st_encabe)
destroy(this.plb_documentos)
destroy(this.dw_3)
end on

event open;Boolean	lb_Cerrar
im_menu		= m_principal

This.Icon									=	Gstr_apl.Icono
This.ParentWindow().ToolBarVisible	=	True
im_menu.Item[1].Item[6].Enabled		=	True
im_menu.Item[7].Visible					=	True

dw_1.SetTransObject(sqlca)
dw_2.SetTransObject(sqlca)
dw_3.SetTransObject(sqlca)

dw_1.Modify("datawindow.message.title='Error '+ is_titulo")
dw_1.SetRowFocusIndicator(Hand!)
//dw_1.Modify("DataWindow.Footer.Height = 180")

istr_mant.dw						=	dw_1
istr_mant.UsuarioSoloConsulta	=	OpcionSoloConsulta()
istr_mant.Solo_Consulta			=	istr_mant.UsuarioSoloConsulta

GrabaAccesoAplicacion(True, id_FechaAcceso, it_HoraAcceso, This.Title, "Acceso a Aplicación", 1)

If IsNull(uo_SelPlanta.Codigo) Then lb_Cerrar = True


If lb_Cerrar Then
	Close(This)
Else
	
	iuo_Camara	=	Create uo_CamarasBode
	
	uo_SelPlanta.Seleccion(False, False)	
	uo_SelPlanta.Codigo = gi_CodPlanta
	uo_SelPlanta.dw_Seleccion.Object.Codigo[1] = gi_CodPlanta
	
	dw_1.GetChild('cama_codigo', idwc_Camara)
	idwc_Camara.SetTransObject(Sqlca)
	idwc_Camara.Retrieve(uo_SelPlanta.Codigo)
	
	dw_1.Retrieve(uo_SelPlanta.Codigo)
	
	RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, is_Ruta)
	
	is_Filtro = '*.csv'

	GetFolder('Seleccion de Directorio', is_Ruta)
	wf_carga_documentos(is_Filtro)
	
End If
end event

event close;Boolean	Valida
Window	ventana
Integer	li_vta

ventana	= This.ParentWindow().GetFirstSheet()

IF IsValid(ventana) THEN
	li_vta++

	DO
		ventana	= This.ParentWindow().GetNextSheet(ventana)
		valida	= IsValid(ventana)
		IF valida THEN li_vta++
	LOOP WHILE valida
END IF

IF li_vta = 1 THEN
	This.ParentWindow().ToolBarVisible	= False
	im_menu.Item[1].Item[6].Enabled		= False
	im_menu.Item[7].Visible					= False
END IF

GrabaAccesoAplicacion(False, id_FechaAcceso, it_HoraAcceso, "", "", 0)
end event

event resize;Integer		li_posic_x, li_posic_y, li_visible, &
				li_Ancho = 300, li_Alto = 245, li_Siguiente = 255

dw_1.Resize(This.WorkSpaceWidth() - 690 - plb_documentos.Width,This.WorkSpaceHeight() - dw_1.y - 75)

dw_1.x						=	78
st_encabe.width			= dw_1.width + plb_documentos.Width + 100
plb_documentos.x			= dw_1.width + 178
plb_documentos.Height	= dw_1.Height

uo_SelPlanta.x			=	(st_encabe.width / 2)
st_1.x						= uo_SelPlanta.x - st_1.width 

IF st_encabe.Visible THEN
	li_posic_y				=	st_encabe.y
ELSE
	li_posic_y				=	dw_1.y
END IF

li_posic_x				=	This.WorkSpaceWidth() - 370

pb_lectura.x				=	li_posic_x
pb_lectura.y				=	li_posic_y
pb_lectura.width		=	li_Ancho
pb_lectura.height		=	li_Alto
li_posic_y 				+= li_Siguiente * 1.25

IF pb_nuevo.Visible THEN
	pb_nuevo.x			=	li_posic_x
	pb_nuevo.y			=	li_posic_y
	pb_nuevo.width	=	li_Ancho
	pb_nuevo.height	=	li_Alto
	li_visible++
	li_posic_y 			+= li_Siguiente
END IF

IF pb_imprimir.Visible THEN
	pb_imprimir.x			=	li_posic_x
	pb_imprimir.y			=	li_posic_y
	pb_imprimir.width		=	li_Ancho
	pb_imprimir.height	=	li_Alto
	li_visible ++
	li_posic_y += li_Siguiente
END IF

pb_salir.x				=	li_posic_x
pb_salir.y				=	dw_1.y + dw_1.Height - li_Siguiente
pb_salir.width			=	li_Ancho
pb_salir.height			=	li_Alto
end event

type dw_2 from uo_dw within w_carga_temperaturas
boolean visible = false
integer x = 3406
integer y = 252
integer width = 224
integer height = 176
integer taborder = 40
string dataobject = "dw_carga_temperaturas"
boolean vscrollbar = false
boolean border = false
end type

type plb_1 from picturelistbox within w_carga_temperaturas
boolean visible = false
integer x = 3351
integer y = 56
integer width = 407
integer height = 176
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean border = false
borderstyle borderstyle = stylelowered!
long picturemaskcolor = 536870912
end type

type st_1 from statictext within w_carga_temperaturas
integer x = 1157
integer y = 120
integer width = 238
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type uo_selplanta from uo_seleccion_plantas within w_carga_temperaturas
event destroy ( )
integer x = 1390
integer y = 112
integer height = 88
integer taborder = 30
boolean bringtotop = true
boolean enabled = false
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.Codigo) Then Return

dw_1.GetChild('cama_codigo', idwc_Camara)
idwc_Camara.SetTransObject(Sqlca)
idwc_Camara.Retrieve(This.Codigo)

dw_1.Retrieve(This.Codigo)
end event

type pb_imprimir from picturebutton within w_carga_temperaturas
string tag = "Selección de Parámetros"
boolean visible = false
integer x = 3547
integer y = 908
integer width = 302
integer height = 244
integer taborder = 50
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Imprimir.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Imprimir-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;Long			ll_Fila, ll_Numero
Integer		li_Cajas= 0
DateTime	ld_Desde, ld_Hasta

SetPointer(HourGlass!)

istr_info.titulo	=	"Consulta Trazabilidad"
istr_info.copias	=	1


OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = ""
vinf.dw_1.SetTransObject(sqlca)
ll_Fila	=	vinf.dw_1.Retrieve()

IF ll_Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ElseIf ll_Fila = 0 THEN 
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
Else
	F_Membrete(vinf.dw_1)
	If gs_Ambiente <> 'Windows' Then F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
End If

SetPointer(Arrow!)
end event

type pb_lectura from picturebutton within w_carga_temperaturas
string tag = "Cambia Directorio de Busqueda"
integer x = 3547
integer y = 640
integer width = 302
integer height = 244
integer taborder = 60
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;GetFolder('Cambio de Directorio', is_Ruta)
wf_carga_documentos(is_Filtro)
end event

type dw_1 from uo_dw within w_carga_temperaturas
integer x = 50
integer y = 336
integer width = 2094
integer height = 1608
integer taborder = 100
boolean titlebar = true
string title = "Consulta Archivos Cargados"
string dataobject = "dw_mues_termometriaenca"
boolean hscrollbar = true
end type

event rbuttondown;//m_consultamovtos	l_Menu
//
//If RowCount() =	0	Then
//	Return
//Else
//	gstr_us.OpcionActiva	=	Parent.ClassName()
//	il_fila 						=	Row
//	This.SetRow(il_fila)
//	
//	l_Menu = Create m_consultamovtos
//	
//	m_consultamovtos.m_m_edicion.consultarecepcion.Visible							=	True
//	l_Menu.m_m_edicion.PopMenu(This.PointerX(),This.PointerY()+750)
//	
//End If
//
end event

type pb_salir from picturebutton within w_carga_temperaturas
integer x = 3547
integer y = 1696
integer width = 302
integer height = 244
integer taborder = 90
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event pb_salir::clicked;call super::clicked;Close(parent)
end event

type pb_nuevo from picturebutton within w_carga_temperaturas
string tag = "Selección de Parámetros"
integer x = 3557
integer y = 1388
integer width = 302
integer height = 244
integer taborder = 80
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 17\Imagenes\Botones\Documento.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Documento-bn.png"
alignment htextalign = left!
long backcolor = 553648127
end type

event clicked;dw_1.Reset()
wf_carga_documentos(is_Filtro)
end event

type st_encabe from statictext within w_carga_temperaturas
integer x = 37
integer y = 48
integer width = 3291
integer height = 232
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type plb_documentos from picturelistbox within w_carga_temperaturas
integer x = 2203
integer y = 336
integer width = 1134
integer height = 1608
integer taborder = 70
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean italic = true
long textcolor = 8388608
boolean hscrollbar = true
boolean vscrollbar = true
integer tabstop[] = {0}
borderstyle borderstyle = stylelowered!
boolean extendedselect = true
long picturemaskcolor = 536870912
end type

event doubleclicked;SetPointer(HourGlass!)

wf_carga_archivos()

SetPointer(Arrow!)
end event

type dw_3 from uo_dw within w_carga_temperaturas
boolean visible = false
integer x = 3675
integer y = 252
integer width = 224
integer height = 176
integer taborder = 80
boolean bringtotop = true
string dataobject = "dw_mues_termometriadeta"
boolean vscrollbar = false
boolean border = false
borderstyle borderstyle = stylebox!
end type

