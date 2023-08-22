$PBExportHeader$w_gene_proceso_pdf.srw
forward
global type w_gene_proceso_pdf from w_para_informes
end type
type dw_1 from uo_dw within w_gene_proceso_pdf
end type
type dw_2 from uo_dw within w_gene_proceso_pdf
end type
type uo_selplanta from uo_seleccion_plantas within w_gene_proceso_pdf
end type
type em_fecha from editmask within w_gene_proceso_pdf
end type
type st_1 from statictext within w_gene_proceso_pdf
end type
type st_2 from statictext within w_gene_proceso_pdf
end type
type dw_3 from uo_dw within w_gene_proceso_pdf
end type
type hpb_progreso from hprogressbar within w_gene_proceso_pdf
end type
type dw_4 from uo_dw within w_gene_proceso_pdf
end type
end forward

global type w_gene_proceso_pdf from w_para_informes
integer x = 14
integer y = 32
integer width = 2953
integer height = 1900
string title = "Proceso Recepción PDF"
boolean controlmenu = false
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "AppIcon!"
event ue_recuperadatos ( )
event ue_asignacion ( )
dw_1 dw_1
dw_2 dw_2
uo_selplanta uo_selplanta
em_fecha em_fecha
st_1 st_1
st_2 st_2
dw_3 dw_3
hpb_progreso hpb_progreso
dw_4 dw_4
end type
global w_gene_proceso_pdf w_gene_proceso_pdf

type variables
String	is_Ruta
uo_docto_recepcion_pdf	iuo_Pdf
uo_recepcionpdf			iuo_Recepcion
uo_Especie		iuo_Especie
end variables

forward prototypes
public subroutine wf_mensaje (datawindow adw, string as_mensaje)
public function boolean wf_grabaimagenes ()
public function boolean wf_graba ()
end prototypes

public subroutine wf_mensaje (datawindow adw, string as_mensaje);Long	ll_New

ll_New	=	adw.InsertRow(0)
adw.Object.errores[ll_New]	=	as_mensaje

adw.ScrollToRow(ll_New)
adw.SelectRow(0, False)
adw.SelectRow(ll_New, True)
adw.SetRow(ll_New)
adw.SetFocus()
	
Return
end subroutine

public function boolean wf_grabaimagenes ();Boolean	lb_Retorno = True
Long		ll_Fila
String		ls_Archivo

If dw_4.RowCount() < 1 Then
	lb_Retorno = False
Else
	For ll_Fila = 1 To dw_4.RowCount()
		If dw_4.Object.genera[ll_Fila] = 0 Then
			ls_Archivo	=	dw_4.Object.recp_rutas[ll_Fila] + '\' + dw_4.Object.recp_archiv[ll_Fila]
			
			iuo_Pdf.GrabaImagen(dw_4, ll_Fila, Sqlca, ls_Archivo)
	
			If Not FileDelete(ls_Archivo) Then
				wf_Mensaje(dw_3, 'No se pudo eliminar archivo: ' + ls_Archivo)
			End If
		End If
	Next
End If

Return lb_Retorno
end function

public function boolean wf_graba ();Boolean	lb_AutoCommit, lb_Retorno
DateTime	ldt_FechaHora

ldt_FechaHora		=	F_FechaHora()
dw_4.GrupoFecha	=	ldt_FechaHora

IF Not dw_4.uf_check_required(0) THEN RETURN False

IF Not dw_4.uf_validate(0) THEN RETURN False

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

IF dw_4.Update(True, False) = 1 then 
	Commit;
	
	IF sqlca.SQLCode <> 0 THEN
		F_ErrorBaseDatos(sqlca, This.Title)
		lb_Retorno	=	False
	ELSE
		lb_Retorno	=	True
			
		dw_4.ResetUpdate()
	END IF
ELSE
	RollBack;
	
	IF sqlca.SQLCode <> 0 THEN F_ErrorBaseDatos(sqlca, This.Title)
	
	lb_Retorno	=	False
END IF

sqlca.AutoCommit	=	lb_AutoCommit

RETURN lb_Retorno
end function

on w_gene_proceso_pdf.create
int iCurrent
call super::create
this.dw_1=create dw_1
this.dw_2=create dw_2
this.uo_selplanta=create uo_selplanta
this.em_fecha=create em_fecha
this.st_1=create st_1
this.st_2=create st_2
this.dw_3=create dw_3
this.hpb_progreso=create hpb_progreso
this.dw_4=create dw_4
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_1
this.Control[iCurrent+2]=this.dw_2
this.Control[iCurrent+3]=this.uo_selplanta
this.Control[iCurrent+4]=this.em_fecha
this.Control[iCurrent+5]=this.st_1
this.Control[iCurrent+6]=this.st_2
this.Control[iCurrent+7]=this.dw_3
this.Control[iCurrent+8]=this.hpb_progreso
this.Control[iCurrent+9]=this.dw_4
end on

on w_gene_proceso_pdf.destroy
call super::destroy
destroy(this.dw_1)
destroy(this.dw_2)
destroy(this.uo_selplanta)
destroy(this.em_fecha)
destroy(this.st_1)
destroy(this.st_2)
destroy(this.dw_3)
destroy(this.hpb_progreso)
destroy(this.dw_4)
end on

event open;call super::open;Boolean	lb_Cerrar

iuo_Especie	=	Create uo_Especie
iuo_Especie.Existe(Integer(Message.StringParm), False, Sqlca)

This.Title	= "PROCESO RECEPCION PDF " + Upper(iuo_Especie.Nombre)
Choose Case iuo_Especie.Codigo
	Case 21
		dw_2.DataObject = "dw_inf_recepcion_pdf"
		dw_2.Modify('DataWindow.Zoom = 90')
	Case 41
		dw_2.DataObject = "dw_inf_recepcion_pdf_kiwis"
		dw_2.Modify('DataWindow.Zoom = 85')
End Choose
	
If IsNull(uo_SelPlanta.Codigo)	Then lb_Cerrar	=	True

If lb_Cerrar Then
	Close(This)
Else	
	uo_SelPlanta.Seleccion(False, False)
	uo_SelPlanta.Codigo = gstr_ParamPlanta.CodigoPlanta
	uo_SelPlanta.dw_Seleccion.Object.codigo[1] =	gstr_ParamPlanta.CodigoPlanta
	em_Fecha.Text = String(Today(), 'dd/mm/yyyy')
	hpb_progreso.Position			= 0
	
	iuo_Pdf			=	Create uo_docto_recepcion_pdf
	iuo_Recepcion	=	Create uo_recepcionpdf
	
	RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, is_Ruta)
	
	If Mid(is_Ruta, Len(is_Ruta), 1) = '\' Then is_Ruta = Mid(is_Ruta, 1, Len(is_Ruta) - 1)
		
	dw_1.SetTransObject(Sqlca)
	dw_2.SetTransObject(Sqlca)
	dw_4.SetTransObject(Sqlca)
	
End If
end event

type pb_excel from w_para_informes`pb_excel within w_gene_proceso_pdf
end type

type st_computador from w_para_informes`st_computador within w_gene_proceso_pdf
integer x = 2007
end type

type st_usuario from w_para_informes`st_usuario within w_gene_proceso_pdf
integer x = 2007
end type

type st_temporada from w_para_informes`st_temporada within w_gene_proceso_pdf
integer x = 2007
end type

type p_logo from w_para_informes`p_logo within w_gene_proceso_pdf
end type

type st_titulo from w_para_informes`st_titulo within w_gene_proceso_pdf
integer width = 2107
integer height = 256
borderstyle borderstyle = styleraised!
end type

type pb_acepta from w_para_informes`pb_acepta within w_gene_proceso_pdf
string tag = "Recupera Informacion"
integer x = 2478
integer y = 696
integer taborder = 200
integer weight = 400
fontcharset fontcharset = ansi!
string picturename = "\Desarrollo 12\Imagenes\Botones\BD_E.bmp"
string disabledname = "\Desarrollo 12\Imagenes\Botones\BD_D.bmp"
string powertiptext = "Recupera Informacion"
end type

event pb_acepta::clicked;Long	ll_Fila, ll_New, ll_Busca
String	ls_Archivo, ls_Busca
Date	ld_Fecha

dw_3.Reset()
dw_4.Reset()
dw_4.Retrieve()

em_Fecha.GetData(ld_Fecha)

If dw_1.Retrieve(uo_SelPlanta.Codigo, ld_Fecha, iuo_Especie.Codigo) = 0 Then
	wf_mensaje(dw_3, "No existen Procesos para esta seleccion.")
	Return
Else
	hpb_progreso.MinPosition		= 0
	hpb_progreso.MaxPosition	= dw_1.RowCount()
	hpb_progreso.Position			= 0
	hpb_progreso.SetStep		= 1
	
	For ll_Fila = 1 To dw_1.RowCount()
		ls_Archivo	=	'P' + String(dw_1.Object.prod_codigo[ll_Fila], '00000') + String(ld_Fecha, 'ddmmyyyy') + '.pdf'
		
		If dw_2.Retrieve(uo_SelPlanta.Codigo, -1, dw_1.Object.prod_codigo[ll_Fila], ld_Fecha, ld_Fecha, iuo_Especie.Codigo) = 0 Then
			wf_Mensaje(dw_3, 'No se encontro informacion para productor: ' + String(dw_1.Object.prod_codigo[ll_Fila], '00000'))
		Else
			dw_2.Object.DataWindow.Zoom = 90
			If dw_2.SaveAs(is_Ruta + '\' +ls_Archivo, PDF!, True) = 1 Then 
				wf_Mensaje(dw_3, 'Se genero documento para productor: ' + String(dw_1.Object.prod_codigo[ll_Fila], '00000'))
				
				ls_Busca = "clie_codigo = " + String(dw_1.Object.clie_codigo[ll_Fila]) + " And plde_codigo = " + &
						String(dw_1.Object.plde_codigo[ll_Fila]) + " And espe_codigo = " + String(dw_1.Object.espe_codigo[ll_Fila]) + &
						" And prod_codigo = " + String(dw_1.Object.prod_codigo[ll_Fila]) +&
						' And String(recp_fechap, "dd/mm/yyyy") = "' + String(ld_Fecha, 'dd/mm/yyyy') + '"'
				
				ll_Busca = dw_4.Find(ls_Busca, 1, dw_4.RowCount())
				
				If ll_Busca > 0 Then
					If MessageBox('Atencion', 'Existe un documento cargado para Productor:' + String(dw_1.Object.prod_codigo[ll_Fila], '00000') + &
							'".~nDesea Reemplazarlo.', StopSign!, YesNo!, 2) = 1 Then
						dw_4.Object.genera[ll_Busca]			=	0
						dw_4.Object.recp_fechac[ll_Busca]	=	Today()
						dw_4.Object.recp_horaca[ll_Busca]	=	Now()
					Else
						wf_Mensaje(dw_3, 'No se reemplazo documento de Productor: ' + String(dw_1.Object.prod_codigo[ll_Fila], '00000'))
					End If
				Else
					ll_New = dw_4.InsertRow(0)
				
					dw_4.Object.clie_codigo[ll_New]	=	dw_1.Object.clie_codigo[ll_Fila]
					dw_4.Object.plde_codigo[ll_New]	=	dw_1.Object.plde_codigo[ll_Fila]
					dw_4.Object.espe_codigo[ll_New]	=	dw_1.Object.espe_codigo[ll_Fila]
					dw_4.Object.prod_codigo[ll_New]	=	dw_1.Object.prod_codigo[ll_Fila]
					dw_4.Object.recp_fechap[ll_New]	=	ld_Fecha
					dw_4.Object.recp_rutas[ll_New]	=	is_Ruta
					dw_4.Object.recp_archiv[ll_New]	=	ls_Archivo
					dw_4.Object.recp_fechac[ll_New]	=	Today()
					dw_4.Object.recp_horaca[ll_New]	=	Now()
					dw_4.Object.genera[ll_New]			=	0
				End If

				If Not wf_Graba() Then
					wf_Mensaje(dw_3, 'No se pudo Grabar registro de Productor: ' + String(dw_1.Object.prod_codigo[ll_Fila], '00000'))
				End If
			Else
				wf_Mensaje(dw_3, 'No se pudo grabar documento PDF para productor: ' + String(dw_1.Object.prod_codigo[ll_Fila], '00000'))
			End If	
		End If
		hpb_progreso.Position	= ll_Fila
	Next
	wf_grabaimagenes()
	wf_Mensaje(dw_3, 'Proceso Terminado Satisfactorimente.')
End If
end event

type pb_salir from w_para_informes`pb_salir within w_gene_proceso_pdf
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2478
integer y = 988
integer taborder = 210
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type dw_1 from uo_dw within w_gene_proceso_pdf
boolean visible = false
integer x = 165
integer y = 1652
integer width = 142
integer height = 104
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_mues_proceso_pdf_productores"
boolean vscrollbar = false
end type

type dw_2 from uo_dw within w_gene_proceso_pdf
boolean visible = false
integer x = 306
integer y = 1652
integer width = 142
integer height = 104
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_inf_recepcion_pdf"
end type

type uo_selplanta from uo_seleccion_plantas within w_gene_proceso_pdf
integer x = 558
integer y = 372
integer height = 80
integer taborder = 40
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

type em_fecha from editmask within w_gene_proceso_pdf
integer x = 1815
integer y = 372
integer width = 480
integer height = 84
integer taborder = 50
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
long backcolor = 16777215
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
boolean dropdowncalendar = true
end type

type st_1 from statictext within w_gene_proceso_pdf
integer x = 357
integer y = 380
integer width = 219
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
string text = "Planta"
boolean focusrectangle = false
end type

type st_2 from statictext within w_gene_proceso_pdf
integer x = 1595
integer y = 380
integer width = 219
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
string text = "Fecha"
boolean focusrectangle = false
end type

type dw_3 from uo_dw within w_gene_proceso_pdf
integer x = 251
integer y = 564
integer width = 2107
integer height = 900
integer taborder = 11
boolean bringtotop = true
boolean titlebar = true
string title = "Estado del Proceso"
string dataobject = "dw_errores"
boolean hscrollbar = true
end type

type hpb_progreso from hprogressbar within w_gene_proceso_pdf
integer x = 251
integer y = 1492
integer width = 2107
integer height = 88
boolean bringtotop = true
unsignedinteger maxposition = 100
unsignedinteger position = 50
integer setstep = 10
end type

type dw_4 from uo_dw within w_gene_proceso_pdf
boolean visible = false
integer x = 462
integer y = 1652
integer width = 169
integer height = 116
integer taborder = 21
boolean bringtotop = true
string dataobject = "dw_mues_recepcion_pdf"
boolean vscrollbar = false
end type

