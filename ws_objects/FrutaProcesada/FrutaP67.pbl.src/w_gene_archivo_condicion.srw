$PBExportHeader$w_gene_archivo_condicion.srw
$PBExportComments$Ventana de Consulta ultima inspeccion
forward
global type w_gene_archivo_condicion from w_para_informes
end type
type st_1 from statictext within w_gene_archivo_condicion
end type
type sle_mensaje from singlelineedit within w_gene_archivo_condicion
end type
type st_2 from statictext within w_gene_archivo_condicion
end type
type dw_1 from uo_dw within w_gene_archivo_condicion
end type
type st_3 from statictext within w_gene_archivo_condicion
end type
type sle_archivo from singlelineedit within w_gene_archivo_condicion
end type
type cb_carga from commandbutton within w_gene_archivo_condicion
end type
type dw_2 from uo_dw within w_gene_archivo_condicion
end type
type uo_selfrigo from uo_seleccion_frigorifico_mod within w_gene_archivo_condicion
end type
type st_44 from statictext within w_gene_archivo_condicion
end type
end forward

global type w_gene_archivo_condicion from w_para_informes
integer x = 14
integer y = 32
integer width = 2473
integer height = 1384
string title = "Ultima Inspeccion"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
sle_mensaje sle_mensaje
st_2 st_2
dw_1 dw_1
st_3 st_3
sle_archivo sle_archivo
cb_carga cb_carga
dw_2 dw_2
uo_selfrigo uo_selfrigo
st_44 st_44
end type
global w_gene_archivo_condicion w_gene_archivo_condicion

type variables
str_busqueda	istr_busq
str_mant 		istr_mant
end variables

forward prototypes
private function boolean wf_grabar ()
end prototypes

private function boolean wf_grabar ();Boolean	lb_AutoCommit, lb_Retorno

lb_AutoCommit		=	sqlca.AutoCommit
sqlca.AutoCommit	=	False

If dw_2.Update(True, False) = 1 Then		
	Commit;
			
	If sqlca.SQLCode <> 0 Then
		F_ErrorBaseDatos(sqlca, This.Title)
				
		RollBack;
	Else
		lb_Retorno	=	True
		dw_2.ResetUpdate()
	End If										
Else
	F_ErrorBaseDatos(sqlca, This.Title)		
	RollBack;
End If

sqlca.AutoCommit	=	lb_AutoCommit

Return lb_Retorno
end function

on w_gene_archivo_condicion.create
int iCurrent
call super::create
this.st_1=create st_1
this.sle_mensaje=create sle_mensaje
this.st_2=create st_2
this.dw_1=create dw_1
this.st_3=create st_3
this.sle_archivo=create sle_archivo
this.cb_carga=create cb_carga
this.dw_2=create dw_2
this.uo_selfrigo=create uo_selfrigo
this.st_44=create st_44
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.sle_mensaje
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.dw_1
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.sle_archivo
this.Control[iCurrent+7]=this.cb_carga
this.Control[iCurrent+8]=this.dw_2
this.Control[iCurrent+9]=this.uo_selfrigo
this.Control[iCurrent+10]=this.st_44
end on

on w_gene_archivo_condicion.destroy
call super::destroy
destroy(this.st_1)
destroy(this.sle_mensaje)
destroy(this.st_2)
destroy(this.dw_1)
destroy(this.st_3)
destroy(this.sle_archivo)
destroy(this.cb_carga)
destroy(this.dw_2)
destroy(this.uo_selfrigo)
destroy(this.st_44)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelFrigo.Codigo) 	THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelFrigo.Seleccion(True, False)
	uo_SelFrigo.dw_Seleccion.Object.codigo.Dddw.PercentWidth	=	190	
END IF
end event

type pb_excel from w_para_informes`pb_excel within w_gene_archivo_condicion
end type

type st_computador from w_para_informes`st_computador within w_gene_archivo_condicion
end type

type st_usuario from w_para_informes`st_usuario within w_gene_archivo_condicion
integer x = 2071
end type

type st_temporada from w_para_informes`st_temporada within w_gene_archivo_condicion
end type

type p_logo from w_para_informes`p_logo within w_gene_archivo_condicion
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_gene_archivo_condicion
integer y = 300
integer width = 1664
string text = "Generacion de Archivo Condición de Calidad"
end type

type pb_acepta from w_para_informes`pb_acepta within w_gene_archivo_condicion
string tag = "Imprimir Reporte"
integer x = 2062
integer y = 496
integer taborder = 20
fontcharset fontcharset = ansi!
boolean underline = true
boolean default = false
string picturename = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Aceptar base datos-bn.png"
end type

event pb_acepta::clicked;SetPointer(HourGlass!)
String	ls_Archivo, ls_Busca, ls_Calibre, ls_embalaje
Long	ll_Busca, ll_Fila

sle_Mensaje.Text	= 'Proceso de Carga de Archivo...'
ls_Archivo			= sle_Archivo.Text

dw_1.SetTransObject(Sqlca)
dw_1.Reset()
dw_1.ImportFile(CSV!, ls_Archivo)

dw_2.SetTransObject(Sqlca)
dw_2.Reset()
dw_2.Retrieve() 

For ll_Fila = 1 To dw_1.RowCount()
	If IsNull(dw_1.Object.vaca_calibr[ll_Fila]) Then
		ls_Calibre = ''
	Else
		ls_Calibre = dw_1.Object.vaca_calibr[ll_Fila]
	End IF
	
	If IsNull(dw_1.Object.emba_codigo[ll_Fila]) Then
		ls_Embalaje = ''
	Else
		ls_Embalaje = dw_1.Object.emba_codigo[ll_Fila]
	End If
	
	ls_Busca = 'clie_codigo = ' + String(dw_1.Object.clie_codigo[ll_Fila]) + ' And plde_codigo = ' + String(dw_1.Object.plde_codigo[ll_Fila]) + ' And etiq_codigo = ' + String(dw_1.Object.etiq_codigo[ll_Fila]) + &
		' And prod_codigo = ' + String(dw_1.Object.prod_codigo[ll_Fila]) + ' and espe_codigo = ' + String(dw_1.Object.espe_codigo[ll_Fila]) + &
		' And vari_codigo = ' + String(dw_1.Object.vari_codigo[ll_Fila]) + ' And upper(vaca_calibr) = "' + ls_Calibre + &
		'" And plde_codpak = ' + String(dw_1.Object.plde_codpak[ll_Fila]) + ' And emba_codigo = "' + ls_Embalaje  + &
		'" and String(cclo_fecemb, "dd/mm/yyyy") = "' + String(dw_1.Object.cclo_fecemb[ll_Fila], 'dd/mm/yyyy') +  '"' +&
		' and ccpd_npalle = ' + String(dw_1.object.cclo_npalle[ll_fila])
		
	ll_Busca = dw_2.Find(ls_Busca, 1, dw_2.RowCount())
	
	If ll_Busca = 0 Then
		dw_1.RowsCopy(ll_Fila, ll_Fila, Primary!, dw_2, 1, Primary!)
	Else
		dw_2.Object.cclo_catcon[ll_Busca] = dw_1.Object.cclo_catcon[ll_Fila]
		dw_2.Object.cclo_fecins[ll_Busca] = dw_1.Object.cclo_fecins[ll_Fila]
		dw_2.Object.cclo_tipori[ll_Busca] = dw_1.Object.cclo_tipori[ll_Fila]
	End If
Next

sle_Mensaje.Text	= 'Proceso de Carga de Archivo Terminado...'
If wf_Grabar() Then sle_Mensaje.Text	= 'Archivo Grabado...'

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_gene_archivo_condicion
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2057
integer y = 776
integer taborder = 30
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_gene_archivo_condicion
integer x = 352
integer y = 532
integer width = 311
integer height = 72
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
boolean enabled = false
string text = "Planta"
boolean focusrectangle = false
end type

type sle_mensaje from singlelineedit within w_gene_archivo_condicion
integer x = 265
integer y = 904
integer width = 1637
integer height = 92
boolean bringtotop = true
integer textsize = -7
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
long textcolor = 65535
long backcolor = 553648127
borderstyle borderstyle = stylelowered!
end type

type st_2 from statictext within w_gene_archivo_condicion
integer x = 279
integer y = 804
integer width = 402
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
string text = "Mensaje"
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type dw_1 from uo_dw within w_gene_archivo_condicion
boolean visible = false
integer x = 1445
integer y = 36
integer width = 174
integer height = 132
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_gene_archivo_condicion"
boolean vscrollbar = false
boolean border = false
end type

type st_3 from statictext within w_gene_archivo_condicion
integer x = 352
integer y = 680
integer width = 311
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
string text = "Archivo"
boolean focusrectangle = false
end type

type sle_archivo from singlelineedit within w_gene_archivo_condicion
integer x = 782
integer y = 668
integer width = 878
integer height = 92
integer taborder = 30
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 33554432
boolean enabled = false
borderstyle borderstyle = stylelowered!
end type

type cb_carga from commandbutton within w_gene_archivo_condicion
integer x = 1669
integer y = 664
integer width = 110
integer height = 92
integer taborder = 40
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "..."
end type

event clicked;String		ls_Ruta, ls_Archivo

If GetFileSaveName ( "Seleccion de Archivo", ls_Ruta, ls_Archivo, "ICS", "Archivos ICS (" + &
			String(uo_SelFrigo.Codigo, '0000')+ "*.ics),*.ics" , "C:", 32770) = 1 Then
   sle_Archivo.Text = ls_Ruta
End If
end event

type dw_2 from uo_dw within w_gene_archivo_condicion
boolean visible = false
integer x = 1710
integer y = 12
integer width = 174
integer height = 132
integer taborder = 10
boolean bringtotop = true
string dataobject = "dw_gene_archvio_condicion_graba"
boolean vscrollbar = false
boolean border = false
end type

type uo_selfrigo from uo_seleccion_frigorifico_mod within w_gene_archivo_condicion
event destroy ( )
integer x = 782
integer y = 496
integer width = 878
integer height = 140
integer taborder = 10
end type

on uo_selfrigo.destroy
call uo_seleccion_frigorifico_mod::destroy
end on

event ue_cambio;call super::ue_cambio;cb_carga.Enabled = True
end event

type st_44 from statictext within w_gene_archivo_condicion
integer x = 251
integer y = 432
integer width = 1664
integer height = 632
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

