$PBExportHeader$w_gene_pronostico_cierre.srw
$PBExportComments$Genera Datos para Pronóstico de Cierre
forward
global type w_gene_pronostico_cierre from window
end type
type dw_1 from uo_dw within w_gene_pronostico_cierre
end type
type uo_seltemporada from uo_seleccion_paramtemporada within w_gene_pronostico_cierre
end type
type uo_selespecie from uo_seleccion_especie within w_gene_pronostico_cierre
end type
type st_6 from statictext within w_gene_pronostico_cierre
end type
type sle_mensa from singlelineedit within w_gene_pronostico_cierre
end type
type st_5 from statictext within w_gene_pronostico_cierre
end type
type pb_salir from picturebutton within w_gene_pronostico_cierre
end type
type pb_acepta from picturebutton within w_gene_pronostico_cierre
end type
type st_1 from statictext within w_gene_pronostico_cierre
end type
type st_titulo from statictext within w_gene_pronostico_cierre
end type
type st_3 from statictext within w_gene_pronostico_cierre
end type
end forward

global type w_gene_pronostico_cierre from window
integer x = 1074
integer y = 484
integer width = 2231
integer height = 1056
boolean titlebar = true
string title = "Generación de Cierre de Pronósticos"
boolean controlmenu = true
boolean minbox = true
boolean maxbox = true
boolean resizable = true
long backcolor = 16777215
dw_1 dw_1
uo_seltemporada uo_seltemporada
uo_selespecie uo_selespecie
st_6 st_6
sle_mensa sle_mensa
st_5 st_5
pb_salir pb_salir
pb_acepta pb_acepta
st_1 st_1
st_titulo st_titulo
st_3 st_3
end type
global w_gene_pronostico_cierre w_gene_pronostico_cierre

type variables

end variables

forward prototypes
public subroutine wf_enviamail ()
end prototypes

public subroutine wf_enviamail ();//Long			ll_Result, ll_Fila
//Integer		li_Semana
//Date			ld_Inicio, ld_Final
//String			ls_Mensaje, ls_ErrorMsg, ls_Para, ls_Archivo
//uo_zonas	luo_zonas
//DataStore	lds_semana
//	
//lds_semana 		= Create DataStore
//	
//lds_semana.DataObject = 'dw_consulta_numerosemana'
//lds_semana.SetTransObject(SQLCA)
//	
//lds_semana.Retrieve(40, uo_SelTemporada.Codigo, uo_SelEspecie.Codigo)
//	
//If lds_semana.Rowcount() > 0 Then 	
//	lds_semana.Retrieve(lds_semana.Object.iniciotemp[1], uo_SelTemporada.Codigo,uo_SelEspecie.Codigo)
//	li_Semana	=	lds_semana.Object.iniciotemp[1]
//	ld_Inicio		=	Date(lds_semana.Object.fech_dlunes[1])
//	
//	lds_semana.Retrieve(lds_semana.Object.iniciotemp[1]-1, uo_SelTemporada.Codigo, uo_SelEspecie.Codigo)
//	ld_Final		=	Date(lds_semana.Object.fech_dlunes[1])
//End If
//	
//Destroy lds_semana
//
//dw_2.DataObject =	"dw_info_comp_estimacionsemanal"
//dw_2.SetTransObject(sqlca)
//dw_2.Retrieve(uo_SelEspecie.Codigo, -1, -1, -1, uo_SelTemporada.Codigo, ld_Inicio, ld_Final, -1, li_Semana)
//
//ls_Archivo = "C:\Cierre_"+ String(Today(), 'yyyymmdd') + ".Pdf"
//
//If dw_2.SaveAs(ls_Archivo, PDF!, True) = -1 Then ls_Archivo = ''
//luo_zonas	=	Create uo_zonas
//
//dw_1.Reset()
//dw_1.Retrieve(gstr_ParamPlanta.CodigoPlanta, -1)
//
//For ll_Fila = 1 To dw_1.RowCount()
//	If ll_Fila = dw_1.RowCount() Then
//		ls_para += '<' + dw_1.Object.ppco_correo[ll_Fila] + '>'
//	Else
//		ls_para += '<' + dw_1.Object.ppco_correo[ll_Fila] + '>;'
//	End If
//Next
//
////luo_zonas.Existe(dw_1.Object.zona_codigo[1], False, Sqlca)
//
//ls_mensaje = 'Señores: ' + &
//				 '~n~nSe ha efectuado Cierre: al ' + uo_selCierre.Nombre + ', ( Nro. ' + String(uo_selCierre.Codigo, '00') + ') - Zona ' + luo_Zonas.nombre + &
//				 '. Para Especie: ' + uo_SelEspecie.Nombre + '~n~n~nActualizado:' + String(Today(), 'dd/mm/yyyy') + ' - ' + String(Now(), 'hh:mm') + ' Hrs.' +&
//				 '~nUsuario~t: ' + gstr_us.Nombre + &
//				 '~nEquipo~t: ' + gstr_us.Computador
//
////ll_Result	= send_mail('smtp.rioblanco.cl', '<estimaciones@rioblanco.cl>', ls_Para,'','', 'Cierre de Pronostico.', ls_mensaje, ls_Archivo, ls_ErrorMsg)
//If (ll_Result < 0) Then
//	MessageBox("Error No" + string(ll_Result), ls_ErrorMsg)
//End If
//
//FileDelete(ls_Archivo)
end subroutine

on w_gene_pronostico_cierre.create
this.dw_1=create dw_1
this.uo_seltemporada=create uo_seltemporada
this.uo_selespecie=create uo_selespecie
this.st_6=create st_6
this.sle_mensa=create sle_mensa
this.st_5=create st_5
this.pb_salir=create pb_salir
this.pb_acepta=create pb_acepta
this.st_1=create st_1
this.st_titulo=create st_titulo
this.st_3=create st_3
this.Control[]={this.dw_1,&
this.uo_seltemporada,&
this.uo_selespecie,&
this.st_6,&
this.sle_mensa,&
this.st_5,&
this.pb_salir,&
this.pb_acepta,&
this.st_1,&
this.st_titulo,&
this.st_3}
end on

on w_gene_pronostico_cierre.destroy
destroy(this.dw_1)
destroy(this.uo_seltemporada)
destroy(this.uo_selespecie)
destroy(this.st_6)
destroy(this.sle_mensa)
destroy(this.st_5)
destroy(this.pb_salir)
destroy(this.pb_acepta)
destroy(this.st_1)
destroy(this.st_titulo)
destroy(this.st_3)
end on

event open;Boolean	lb_Cerrar

If IsNull(uo_SelEspecie.codigo) 	Then lb_Cerrar	=	True
If IsNull(uo_SelTemporada.codigo)Then lb_Cerrar	=	True
If lb_Cerrar Then
	Close(This)
Else
	uo_SelTemporada.Seleccion(False, False)
	uo_selEspecie.Seleccion(False, False)
	
	uo_SelTemporada.Inicia(gstr_tempo.temporada)
	uo_SelEspecie.Inicia(11)
	
	dw_1.SetTransObject(Sqlca)
End If
end event

event mousemove;IF(IsValid(w_main))Then
	w_main.SetMicroHelp("Ventana : " + ClassName())
End if

end event

type dw_1 from uo_dw within w_gene_pronostico_cierre
boolean visible = false
integer x = 1746
integer width = 151
integer height = 132
integer taborder = 70
string dataobject = "dw_mant_mues_paramcorreo"
boolean vscrollbar = false
end type

type uo_seltemporada from uo_seleccion_paramtemporada within w_gene_pronostico_cierre
event destroy ( )
integer x = 567
integer y = 320
integer height = 80
integer taborder = 10
end type

on uo_seltemporada.destroy
call uo_seleccion_paramtemporada::destroy
end on

type uo_selespecie from uo_seleccion_especie within w_gene_pronostico_cierre
event destroy ( )
integer x = 567
integer y = 480
integer height = 80
integer taborder = 20
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type st_6 from statictext within w_gene_pronostico_cierre
integer x = 165
integer y = 320
integer width = 370
integer height = 84
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
string text = "Temporada"
boolean focusrectangle = false
end type

type sle_mensa from singlelineedit within w_gene_pronostico_cierre
integer x = 119
integer y = 700
integer width = 1577
integer height = 132
integer textsize = -14
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = script!
string facename = "Comic Sans MS"
long textcolor = 65535
long backcolor = 16711680
boolean autohscroll = false
borderstyle borderstyle = stylelowered!
end type

type st_5 from statictext within w_gene_pronostico_cierre
integer x = 82
integer y = 672
integer width = 1655
integer height = 188
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type pb_salir from picturebutton within w_gene_pronostico_cierre
integer x = 1842
integer y = 616
integer width = 302
integer height = 244
integer taborder = 50
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean cancel = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Apagar.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Apagar-bn.png"
alignment htextalign = right!
end type

event clicked;Close(Parent)
end event

type pb_acepta from picturebutton within w_gene_pronostico_cierre
event clicked pbm_bnclicked
integer x = 1842
integer y = 228
integer width = 302
integer height = 244
integer taborder = 40
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean default = true
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
alignment htextalign = right!
end type

event clicked;SetPointer(HourGlass!)

sle_mensa.text	= "Procesando Cierre de Especie:" + uo_SelEspecie.Nombre
	
Declare Genera_Pronostico_Cierre Procedure For dbo.Pron_Genera_Pronostico_Cierre
			@Temporada 	= :uo_Seltemporada.Codigo,
			@Especie		= :uo_SelEspecie.Codigo
		Using SQLCA ;
		
Execute Genera_Pronostico_Cierre;	

If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca,"Lectura del Procedimiento Almacenado FProc_Genera_Pronostico_Cierre" )
	sle_mensa.text	= "Error al Generar Datos."
	Close Genera_Pronostico_Cierre;
	SetPointer(Arrow!)
	Return -1
End If	

sle_mensa.text	= "Proceso Terminado Satisfactoriamente."
Close   Genera_Pronostico_Cierre;

//wf_EnviaMail()

SetPointer(Arrow!)
end event

type st_1 from statictext within w_gene_pronostico_cierre
integer x = 165
integer y = 480
integer width = 370
integer height = 84
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
boolean focusrectangle = false
end type

type st_titulo from statictext within w_gene_pronostico_cierre
integer x = 82
integer y = 68
integer width = 1655
integer height = 92
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Tahoma"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
string text = "Genera Cierre de Pronóstico"
alignment alignment = center!
boolean border = true
borderstyle borderstyle = stylelowered!
boolean focusrectangle = false
end type

type st_3 from statictext within w_gene_pronostico_cierre
integer x = 82
integer y = 228
integer width = 1655
integer height = 444
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 16711680
boolean enabled = false
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

