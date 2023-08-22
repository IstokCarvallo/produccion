$PBExportHeader$w_carga_temperaturaprefrio.srw
$PBExportComments$Informe Consolidado General de Embarques.
forward
global type w_carga_temperaturaprefrio from w_para_informes
end type
type st_1 from statictext within w_carga_temperaturaprefrio
end type
type mle_texto from multilineedit within w_carga_temperaturaprefrio
end type
type cbx_genera from checkbox within w_carga_temperaturaprefrio
end type
end forward

global type w_carga_temperaturaprefrio from w_para_informes
integer width = 3214
integer height = 1584
event ue_listo ( )
st_1 st_1
mle_texto mle_texto
cbx_genera cbx_genera
end type
global w_carga_temperaturaprefrio w_carga_temperaturaprefrio

type variables

end variables

event ue_listo();w_main.SetMicroHelp("Listo")
SetPointer(Arrow!)

end event

on w_carga_temperaturaprefrio.create
int iCurrent
call super::create
this.st_1=create st_1
this.mle_texto=create mle_texto
this.cbx_genera=create cbx_genera
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_1
this.Control[iCurrent+2]=this.mle_texto
this.Control[iCurrent+3]=this.cbx_genera
end on

on w_carga_temperaturaprefrio.destroy
call super::destroy
destroy(this.st_1)
destroy(this.mle_texto)
destroy(this.cbx_genera)
end on

event open;call super::open;mle_texto.Text = 'Preparando Carga de Informacion pre-frio...' + Char(13)  + Char(10) 
end event

type pb_excel from w_para_informes`pb_excel within w_carga_temperaturaprefrio
integer x = 2789
integer y = 1084
end type

type st_computador from w_para_informes`st_computador within w_carga_temperaturaprefrio
end type

type st_usuario from w_para_informes`st_usuario within w_carga_temperaturaprefrio
end type

type st_temporada from w_para_informes`st_temporada within w_carga_temperaturaprefrio
end type

type p_logo from w_para_informes`p_logo within w_carga_temperaturaprefrio
string picturename = "\Desarrollo 17\Imagenes\Logos\RBlanco.jpg"
end type

type st_titulo from w_para_informes`st_titulo within w_carga_temperaturaprefrio
integer width = 2318
string text = "Carga temperaturas Prefrio"
end type

type pb_acepta from w_para_informes`pb_acepta within w_carga_temperaturaprefrio
string tag = "Imprimir Reporte"
integer x = 2770
integer y = 428
integer taborder = 110
end type

event pb_acepta::clicked;Long	ll_Tipo = 0

SetPointer(Arrow!)

If cbx_genera.Checked Then ll_Tipo = 1

If ll_Tipo = 0 Then
	mle_Texto.Text += 'Comenzando proceso de carga de Temperaturas.' + Char(13)  + Char(10) 
Else
	mle_Texto.Text += 'Comenzando proceso de actualización de Temperaturas.' + Char(13)  + Char(10) 
End If

mle_Texto.Text += 'Inicio Proceso: ' + String(Today(), 'dd/mm/yyyy hh:mm:ss')  + Char(13)  + Char(10) 

DECLARE Carga PROCEDURE FOR dbo.Traz_CargaTemperaturasPrefrio @Tipo = :ll_Tipo;
			  
EXECUTE Carga;
		
If sqlca.SQLCode = -1 Then
	mle_Texto.Text += 'Error en Proceso de Carga.' + Char(13)  + Char(10) 
	Commit;
	Return
End If	
	
Commit;

mle_Texto.Text += 'Termino Proceso: ' + String(Today(), 'dd/mm/yyyy hh:mm:ss')  + Char(13)  + Char(10) 
mle_Texto.Text += 'El Proceso a Concluído Satisfactoriamente.'  + Char(13)  + Char(10) 


SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_carga_temperaturaprefrio
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 2770
integer y = 716
integer taborder = 120
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type st_1 from statictext within w_carga_temperaturaprefrio
integer x = 251
integer y = 432
integer width = 2318
integer height = 952
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 16711680
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type mle_texto from multilineedit within w_carga_temperaturaprefrio
integer x = 274
integer y = 540
integer width = 2272
integer height = 820
integer taborder = 120
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
long textcolor = 8388608
long backcolor = 16777215
boolean vscrollbar = true
borderstyle borderstyle = stylelowered!
end type

type cbx_genera from checkbox within w_carga_temperaturaprefrio
integer x = 306
integer y = 452
integer width = 731
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 553648127
string text = "Regenera Información"
end type

