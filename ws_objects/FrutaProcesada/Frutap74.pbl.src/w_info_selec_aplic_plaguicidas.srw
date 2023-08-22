$PBExportHeader$w_info_selec_aplic_plaguicidas.srw
forward
global type w_info_selec_aplic_plaguicidas from w_para_informes
end type
type st_4 from statictext within w_info_selec_aplic_plaguicidas
end type
type rb_1 from radiobutton within w_info_selec_aplic_plaguicidas
end type
type rb_2 from radiobutton within w_info_selec_aplic_plaguicidas
end type
end forward

global type w_info_selec_aplic_plaguicidas from w_para_informes
integer x = 14
integer y = 32
integer width = 2551
integer height = 1240
string title = "Informes Aplicación Plaguicidas"
boolean minbox = false
string icon = "F:\Desarrollo\Productiva\FrutaProcesada\Informes.ico"
st_4 st_4
rb_1 rb_1
rb_2 rb_2
end type
global w_info_selec_aplic_plaguicidas w_info_selec_aplic_plaguicidas

type variables
Integer codigo
String  nombre

str_mant istr_mant

end variables

forward prototypes
public function boolean existeoperacion (long li_operacion)
public function string buscdescfruta (integer fruta)
end prototypes

public function boolean existeoperacion (long li_operacion);//Date		ld_fzarpe
//integer  li_cliente
//
//IF li_operacion > 0 THEN
//	SELECT	DISTINCT  emb.clie_codigo
//		INTO  :li_cliente
//		FROM	dbo.embarqueprod as emb,dbo.recibidores as rec 
//		WHERE	emb.oper_codigo = :li_operacion and
//				emb.reci_codigo = rec.reci_codigo and
//				emb.clie_codigo = :gi_codexport;
//				
//	IF sqlca.SQLCode = -1 THEN
//		F_ErrorBaseDatos(sqlca,"Lectura de la Tabla OPERACION")
//		em_1.SetFocus()
//		
//		RETURN False
//	ELSEIF sqlca.SQLCode = 100 THEN
//		MessageBox("Atención", "No existe Operación Indicada.~r~rIngrese otro Número.", &
//						Exclamation!, Ok!)
//		
//		em_1.SetFocus()
//		RETURN False
//	ELSE
//		idwc_recibidor_operacion.Reset()
//		idwc_recibidor_operacion.SetTransObject(sqlca)
//		idwc_recibidor_operacion.Retrieve(li_operacion)
//		
//	RETURN True
//	END IF
//ELSE
//	MessageBox("Atención", "Faltan parámetros de búsqueda.~r~rIngreselos todos.", &
//					Exclamation!, Ok!)
	RETURN False
//END IF

end function

public function string buscdescfruta (integer fruta);Integer	li_codigo
String	ls_descri, ls_abrevi

SELECT frre_codigo,frre_descri,frre_abrevi  
INTO :li_codigo,:ls_descri,:ls_abrevi  
FROM dbo.frutarecibida  
WHERE frre_codigo = :fruta;

		
RETURN ls_descri

end function

on w_info_selec_aplic_plaguicidas.create
int iCurrent
call super::create
this.st_4=create st_4
this.rb_1=create rb_1
this.rb_2=create rb_2
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_4
this.Control[iCurrent+2]=this.rb_1
this.Control[iCurrent+3]=this.rb_2
end on

on w_info_selec_aplic_plaguicidas.destroy
call super::destroy
destroy(this.st_4)
destroy(this.rb_1)
destroy(this.rb_2)
end on

event open;istr_mant = Message.PowerObjectParm





end event

type pb_excel from w_para_informes`pb_excel within w_info_selec_aplic_plaguicidas
end type

type st_computador from w_para_informes`st_computador within w_info_selec_aplic_plaguicidas
end type

type st_usuario from w_para_informes`st_usuario within w_info_selec_aplic_plaguicidas
end type

type st_temporada from w_para_informes`st_temporada within w_info_selec_aplic_plaguicidas
end type

type p_logo from w_para_informes`p_logo within w_info_selec_aplic_plaguicidas
end type

type st_titulo from w_para_informes`st_titulo within w_info_selec_aplic_plaguicidas
integer width = 1751
string text = "Informe Recepción"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_selec_aplic_plaguicidas
integer x = 2162
integer y = 440
integer taborder = 50
end type

event pb_acepta::clicked;SetPointer(HourGlass!)

Long		fila
str_info	lstr_info

lstr_info.titulo	= "APLICACIÓN DECLARACIÓN DE PLAGUICIDAS"
lstr_info.copias	= 1

OpenWithParm(vinf,lstr_info)

IF rb_1.Checked THEN
	vinf.dw_1.DataObject = "dw_info_aplic_plaguicidas_anexo_sag"
ELSE
	vinf.dw_1.DataObject = "dw_info_aplic_plaguicidas"
END IF
vinf.dw_1.SetTransObject(sqlca)

fila = vinf.dw_1.Retrieve(Integer(istr_mant.argumento[3]),  Integer(istr_mant.argumento[1]), &
										(istr_mant.argumento[4]),Integer(istr_mant.argumento[2]),(istr_mant.argumento[5]))
IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
					"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", StopSign!, Ok!)
ELSE
	F_Membrete(vinf.dw_1)
	IF gs_Ambiente <> 'Windows' THEN F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_selec_aplic_plaguicidas
integer x = 2162
integer y = 708
integer taborder = 60
end type

type st_4 from statictext within w_info_selec_aplic_plaguicidas
integer x = 251
integer y = 456
integer width = 1751
integer height = 520
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
boolean enabled = false
alignment alignment = center!
boolean border = true
boolean focusrectangle = false
end type

type rb_1 from radiobutton within w_info_selec_aplic_plaguicidas
integer x = 663
integer y = 584
integer width = 837
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
string text = "Formato Declaración SAG"
boolean checked = true
end type

type rb_2 from radiobutton within w_info_selec_aplic_plaguicidas
integer x = 667
integer y = 712
integer width = 576
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
string text = "Listado Revisión "
end type

