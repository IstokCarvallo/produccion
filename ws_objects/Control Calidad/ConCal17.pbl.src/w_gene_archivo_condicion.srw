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
type uo_selespecie from uo_seleccion_especie within w_gene_archivo_condicion
end type
type st_44 from statictext within w_gene_archivo_condicion
end type
type uo_selplanta from uo_seleccion_plantas within w_gene_archivo_condicion
end type
end forward

global type w_gene_archivo_condicion from w_para_informes
integer x = 14
integer y = 32
integer width = 2505
integer height = 1404
string title = "Ultima Inspeccion"
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
st_1 st_1
sle_mensaje sle_mensaje
st_2 st_2
dw_1 dw_1
st_3 st_3
uo_selespecie uo_selespecie
st_44 st_44
uo_selplanta uo_selplanta
end type
global w_gene_archivo_condicion w_gene_archivo_condicion

type variables

end variables

on w_gene_archivo_condicion.create
int iCurrent
call super::create
this.st_1=create st_1
this.sle_mensaje=create sle_mensaje
this.st_2=create st_2
this.dw_1=create dw_1
this.st_3=create st_3
this.uo_selespecie=create uo_selespecie
this.st_44=create st_44
this.uo_selplanta=create uo_selplanta
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.sle_mensaje
this.Control[iCurrent+3]=this.st_2
this.Control[iCurrent+4]=this.dw_1
this.Control[iCurrent+5]=this.st_3
this.Control[iCurrent+6]=this.uo_selespecie
this.Control[iCurrent+7]=this.st_44
this.Control[iCurrent+8]=this.uo_selplanta
end on

on w_gene_archivo_condicion.destroy
call super::destroy
destroy(this.st_1)
destroy(this.sle_mensaje)
destroy(this.st_2)
destroy(this.dw_1)
destroy(this.st_3)
destroy(this.uo_selespecie)
destroy(this.st_44)
destroy(this.uo_selplanta)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelPlanta.Codigo) 		THEN lb_Cerrar	=	True
IF IsNull(uo_SelEspecie.Codigo) 	THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelPlanta.Seleccion(False, False)
	uo_SelEspecie.Seleccion(False, False)
	uo_SelPlanta.Filtra(1)
	uo_SelPlanta.Inicia(gi_CodPlanta)
END IF
end event

type pb_excel from w_para_informes`pb_excel within w_gene_archivo_condicion
end type

type st_computador from w_para_informes`st_computador within w_gene_archivo_condicion
integer x = 1481
end type

type st_usuario from w_para_informes`st_usuario within w_gene_archivo_condicion
integer x = 1481
end type

type st_temporada from w_para_informes`st_temporada within w_gene_archivo_condicion
integer x = 1481
end type

type p_logo from w_para_informes`p_logo within w_gene_archivo_condicion
end type

type st_titulo from w_para_informes`st_titulo within w_gene_archivo_condicion
integer width = 1664
string text = "Generacion de Archivo Condición de Calidad"
end type

type pb_acepta from w_para_informes`pb_acepta within w_gene_archivo_condicion
string tag = "Imprimir Reporte"
integer x = 2062
integer y = 428
integer taborder = 20
integer weight = 400
fontcharset fontcharset = ansi!
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

String 	ls_Ruta, ls_Archivo

sle_Mensaje.Text = 'Proceso de generacion de Archivo...'

dw_1.SetTransObject(Sqlca)
dw_1.Reset()

RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_Ruta)
If Mid(ls_Ruta, Len(ls_Ruta), 1) = '\' Then ls_Ruta = Mid(ls_Ruta, 1, Len(ls_Ruta) - 1)

ls_Archivo = String(uo_SelPlanta.Codigo, '0000') + String(Today(), 'ddmmyyyy') + '.ics'

If dw_1.Retrieve(uo_SelPlanta.Codigo, uo_SelEspecie.Codigo) > 0 Then
	If dw_1.SaveAs(ls_Ruta + '\' + ls_Archivo, CSV!, False) = -1 Then
		sle_Mensaje.Text = 'No se pudo generar archivo: ' + ls_Archivo
	Else
		sle_Mensaje.Text = 'Archivo generado con exito: ' + ls_Ruta + '\' + ls_Archivo
	End If	
Else
	sle_Mensaje.Text = 'No se pudo generar archivo: ' + ls_Archivo
End If

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_gene_archivo_condicion
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2057
integer y = 708
integer taborder = 30
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_gene_archivo_condicion
integer x = 357
integer y = 588
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
long bordercolor = 8388608
boolean focusrectangle = false
end type

type sle_mensaje from singlelineedit within w_gene_archivo_condicion
integer x = 279
integer y = 936
integer width = 1614
integer height = 112
boolean bringtotop = true
integer textsize = -12
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
integer y = 856
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
long bordercolor = 8388608
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type dw_1 from uo_dw within w_gene_archivo_condicion
boolean visible = false
integer y = 364
integer width = 197
integer height = 160
integer taborder = 11
boolean bringtotop = true
string dataobject = "dw_gene_archivo_condicion"
boolean vscrollbar = false
boolean border = false
end type

type st_3 from statictext within w_gene_archivo_condicion
integer x = 352
integer y = 692
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
string text = "Especie"
long bordercolor = 8388608
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_gene_archivo_condicion
integer x = 782
integer y = 688
integer height = 84
integer taborder = 20
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type st_44 from statictext within w_gene_archivo_condicion
integer x = 251
integer y = 440
integer width = 1664
integer height = 632
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type uo_selplanta from uo_seleccion_plantas within w_gene_archivo_condicion
integer x = 782
integer y = 584
integer height = 84
integer taborder = 30
boolean bringtotop = true
end type

on uo_selplanta.destroy
call uo_seleccion_plantas::destroy
end on

