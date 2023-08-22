$PBExportHeader$w_info_existencia_estadia_dias_pallet.srw
forward
global type w_info_existencia_estadia_dias_pallet from w_para_informes
end type
type dw_cliente from datawindow within w_info_existencia_estadia_dias_pallet
end type
type st_6 from statictext within w_info_existencia_estadia_dias_pallet
end type
type st_4 from statictext within w_info_existencia_estadia_dias_pallet
end type
type st_especie from statictext within w_info_existencia_estadia_dias_pallet
end type
type st_nro2 from statictext within w_info_existencia_estadia_dias_pallet
end type
type st_variedad from statictext within w_info_existencia_estadia_dias_pallet
end type
type st_productor from statictext within w_info_existencia_estadia_dias_pallet
end type
type em_productor from editmask within w_info_existencia_estadia_dias_pallet
end type
type cb_buscaproductor from commandbutton within w_info_existencia_estadia_dias_pallet
end type
type sle_productor from singlelineedit within w_info_existencia_estadia_dias_pallet
end type
type cbx_productor from checkbox within w_info_existencia_estadia_dias_pallet
end type
type st_planta from statictext within w_info_existencia_estadia_dias_pallet
end type
type cbx_planta from checkbox within w_info_existencia_estadia_dias_pallet
end type
type dw_planta from datawindow within w_info_existencia_estadia_dias_pallet
end type
type gb_5 from groupbox within w_info_existencia_estadia_dias_pallet
end type
type st_5 from statictext within w_info_existencia_estadia_dias_pallet
end type
type rb_embalaje from radiobutton within w_info_existencia_estadia_dias_pallet
end type
type rb_productor from radiobutton within w_info_existencia_estadia_dias_pallet
end type
type rb_variedad from radiobutton within w_info_existencia_estadia_dias_pallet
end type
type st_7 from statictext within w_info_existencia_estadia_dias_pallet
end type
type cbx_consolplan from checkbox within w_info_existencia_estadia_dias_pallet
end type
type cbx_consolprod from checkbox within w_info_existencia_estadia_dias_pallet
end type
type rb_controltodos from radiobutton within w_info_existencia_estadia_dias_pallet
end type
type rb_rechazados from radiobutton within w_info_existencia_estadia_dias_pallet
end type
type rb_objetados from radiobutton within w_info_existencia_estadia_dias_pallet
end type
type rb_habilitado from radiobutton within w_info_existencia_estadia_dias_pallet
end type
type gb_6 from groupbox within w_info_existencia_estadia_dias_pallet
end type
type st_2 from statictext within w_info_existencia_estadia_dias_pallet
end type
type uo_selcondicion from uo_seleccion_condicion within w_info_existencia_estadia_dias_pallet
end type
type cbx_predio from checkbox within w_info_existencia_estadia_dias_pallet
end type
type st_3 from statictext within w_info_existencia_estadia_dias_pallet
end type
type dw_stat from datawindow within w_info_existencia_estadia_dias_pallet
end type
type cbx_status from checkbox within w_info_existencia_estadia_dias_pallet
end type
type uo_selespecie from uo_seleccion_especie within w_info_existencia_estadia_dias_pallet
end type
type uo_selvariedad from uo_seleccion_variedad within w_info_existencia_estadia_dias_pallet
end type
type cbx_varirotula from checkbox within w_info_existencia_estadia_dias_pallet
end type
type st_8 from statictext within w_info_existencia_estadia_dias_pallet
end type
type em_fecha from editmask within w_info_existencia_estadia_dias_pallet
end type
type st_9 from statictext within w_info_existencia_estadia_dias_pallet
end type
type st_10 from statictext within w_info_existencia_estadia_dias_pallet
end type
type st_11 from statictext within w_info_existencia_estadia_dias_pallet
end type
type em_dias from editmask within w_info_existencia_estadia_dias_pallet
end type
type gb_4 from groupbox within w_info_existencia_estadia_dias_pallet
end type
type st_1 from statictext within w_info_existencia_estadia_dias_pallet
end type
type st_12 from statictext within w_info_existencia_estadia_dias_pallet
end type
type rb_1 from radiobutton within w_info_existencia_estadia_dias_pallet
end type
type rb_2 from radiobutton within w_info_existencia_estadia_dias_pallet
end type
type rb_3 from radiobutton within w_info_existencia_estadia_dias_pallet
end type
type cbx_consolida from checkbox within w_info_existencia_estadia_dias_pallet
end type
type pb_pdf from picturebutton within w_info_existencia_estadia_dias_pallet
end type
type dw_pdf from datawindow within w_info_existencia_estadia_dias_pallet
end type
end forward

global type w_info_existencia_estadia_dias_pallet from w_para_informes
integer x = 14
integer y = 32
integer width = 4448
integer height = 1748
string title = "Existencia de Fruta"
boolean minbox = false
boolean maxbox = false
boolean resizable = false
windowtype windowtype = response!
string icon = "F:\Desarrollo\Producción\FrutaProcesada\Producc.ico"
dw_cliente dw_cliente
st_6 st_6
st_4 st_4
st_especie st_especie
st_nro2 st_nro2
st_variedad st_variedad
st_productor st_productor
em_productor em_productor
cb_buscaproductor cb_buscaproductor
sle_productor sle_productor
cbx_productor cbx_productor
st_planta st_planta
cbx_planta cbx_planta
dw_planta dw_planta
gb_5 gb_5
st_5 st_5
rb_embalaje rb_embalaje
rb_productor rb_productor
rb_variedad rb_variedad
st_7 st_7
cbx_consolplan cbx_consolplan
cbx_consolprod cbx_consolprod
rb_controltodos rb_controltodos
rb_rechazados rb_rechazados
rb_objetados rb_objetados
rb_habilitado rb_habilitado
gb_6 gb_6
st_2 st_2
uo_selcondicion uo_selcondicion
cbx_predio cbx_predio
st_3 st_3
dw_stat dw_stat
cbx_status cbx_status
uo_selespecie uo_selespecie
uo_selvariedad uo_selvariedad
cbx_varirotula cbx_varirotula
st_8 st_8
em_fecha em_fecha
st_9 st_9
st_10 st_10
st_11 st_11
em_dias em_dias
gb_4 gb_4
st_1 st_1
st_12 st_12
rb_1 rb_1
rb_2 rb_2
rb_3 rb_3
cbx_consolida cbx_consolida
pb_pdf pb_pdf
dw_pdf dw_pdf
end type
global w_info_existencia_estadia_dias_pallet w_info_existencia_estadia_dias_pallet

type variables
str_busqueda istr_busq
str_mant istr_mant


DataWindowChild	idwc_cliente,idwc_planta

uo_seleccion_especie		iuo_selespecie
uo_seleccion_variedad	iuo_selvariedad

String is_NomPlanta,is_NomProductor, is_control_d, is_asunto, is_archivo
integer ii_control
end variables

forward prototypes
public function boolean noexisteplanta (string planta)
public function boolean noexistecliente (string cliente)
public subroutine enviamail ()
end prototypes

public function boolean noexisteplanta (string planta);Integer		 li_planta

li_planta	=	Integer(planta)

SELECT	plde_nombre
	INTO	:is_NomPlanta
	FROM	dba.plantadesp
	WHERE	plde_codigo	=	:li_planta;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla PlantaDesp")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Planta no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	dw_planta.SetItem(1, "plde_codigo", li_planta)
	istr_mant.argumento[5]	=	String(li_planta)	
	RETURN False
END IF
end function

public function boolean noexistecliente (string cliente);Integer		li_cliente

String ls_nombre
li_cliente	=	Integer(cliente)

SELECT	clie_nombre
	INTO	:ls_nombre
	FROM	dba.clientesprod
	WHERE	clie_codigo	=	:li_cliente;
		
IF sqlca.SQLCode = -1 THEN
	F_ErrorBaseDatos(sqlca, "Lectura de tabla ClientesProd")
	RETURN True
ELSEIF sqlca.SQLCode = 100 THEN
	MessageBox("Atención", "Código de Cliente no ha sido Definido.~r~r" + &
		"Ingrese o seleccione otro Código.")
	RETURN True
ELSE
	dw_cliente.SetItem(1, "clie_codigo", li_cliente)
	istr_mant.argumento[1]	=	String(li_cliente)	
	RETURN False
END IF
end function

public subroutine enviamail ();String		ls_Nombre, ls_NomReporte, ls_Archivo, ls_DirectorioAct 
Long			ll_Fila, ll_consignatario, ll_Archivo
Boolean		lb_Existe
Integer		li_imprimio
str_parms	lstr_parms

SetPointer(HourGlass!)

//RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_DirectorioAct)

ls_NomReporte									=	is_archivo
lstr_parms.string_arg[1]					=	String(1)
lstr_parms.string_arg[2]					=	is_asunto
lstr_parms.string_arg[3]					=	String(1)

ll_Archivo = 1

lstr_parms.string_arg[ll_Archivo+3]		=	is_archivo


OpenWithParm(w_correo_archivo_existenciapdf, lstr_parms)

lb_Existe	=	FileExists(lstr_parms.string_arg[ll_Archivo + 3])
	
//IF lb_Existe THEN
//	FileDelete(lstr_parms.string_arg[ll_Archivo + 3])
//END IF

SetPointer(Arrow!)
end subroutine

on w_info_existencia_estadia_dias_pallet.create
int iCurrent
call super::create
this.dw_cliente=create dw_cliente
this.st_6=create st_6
this.st_4=create st_4
this.st_especie=create st_especie
this.st_nro2=create st_nro2
this.st_variedad=create st_variedad
this.st_productor=create st_productor
this.em_productor=create em_productor
this.cb_buscaproductor=create cb_buscaproductor
this.sle_productor=create sle_productor
this.cbx_productor=create cbx_productor
this.st_planta=create st_planta
this.cbx_planta=create cbx_planta
this.dw_planta=create dw_planta
this.gb_5=create gb_5
this.st_5=create st_5
this.rb_embalaje=create rb_embalaje
this.rb_productor=create rb_productor
this.rb_variedad=create rb_variedad
this.st_7=create st_7
this.cbx_consolplan=create cbx_consolplan
this.cbx_consolprod=create cbx_consolprod
this.rb_controltodos=create rb_controltodos
this.rb_rechazados=create rb_rechazados
this.rb_objetados=create rb_objetados
this.rb_habilitado=create rb_habilitado
this.gb_6=create gb_6
this.st_2=create st_2
this.uo_selcondicion=create uo_selcondicion
this.cbx_predio=create cbx_predio
this.st_3=create st_3
this.dw_stat=create dw_stat
this.cbx_status=create cbx_status
this.uo_selespecie=create uo_selespecie
this.uo_selvariedad=create uo_selvariedad
this.cbx_varirotula=create cbx_varirotula
this.st_8=create st_8
this.em_fecha=create em_fecha
this.st_9=create st_9
this.st_10=create st_10
this.st_11=create st_11
this.em_dias=create em_dias
this.gb_4=create gb_4
this.st_1=create st_1
this.st_12=create st_12
this.rb_1=create rb_1
this.rb_2=create rb_2
this.rb_3=create rb_3
this.cbx_consolida=create cbx_consolida
this.pb_pdf=create pb_pdf
this.dw_pdf=create dw_pdf
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.dw_cliente
this.Control[iCurrent+2]=this.st_6
this.Control[iCurrent+3]=this.st_4
this.Control[iCurrent+4]=this.st_especie
this.Control[iCurrent+5]=this.st_nro2
this.Control[iCurrent+6]=this.st_variedad
this.Control[iCurrent+7]=this.st_productor
this.Control[iCurrent+8]=this.em_productor
this.Control[iCurrent+9]=this.cb_buscaproductor
this.Control[iCurrent+10]=this.sle_productor
this.Control[iCurrent+11]=this.cbx_productor
this.Control[iCurrent+12]=this.st_planta
this.Control[iCurrent+13]=this.cbx_planta
this.Control[iCurrent+14]=this.dw_planta
this.Control[iCurrent+15]=this.gb_5
this.Control[iCurrent+16]=this.st_5
this.Control[iCurrent+17]=this.rb_embalaje
this.Control[iCurrent+18]=this.rb_productor
this.Control[iCurrent+19]=this.rb_variedad
this.Control[iCurrent+20]=this.st_7
this.Control[iCurrent+21]=this.cbx_consolplan
this.Control[iCurrent+22]=this.cbx_consolprod
this.Control[iCurrent+23]=this.rb_controltodos
this.Control[iCurrent+24]=this.rb_rechazados
this.Control[iCurrent+25]=this.rb_objetados
this.Control[iCurrent+26]=this.rb_habilitado
this.Control[iCurrent+27]=this.gb_6
this.Control[iCurrent+28]=this.st_2
this.Control[iCurrent+29]=this.uo_selcondicion
this.Control[iCurrent+30]=this.cbx_predio
this.Control[iCurrent+31]=this.st_3
this.Control[iCurrent+32]=this.dw_stat
this.Control[iCurrent+33]=this.cbx_status
this.Control[iCurrent+34]=this.uo_selespecie
this.Control[iCurrent+35]=this.uo_selvariedad
this.Control[iCurrent+36]=this.cbx_varirotula
this.Control[iCurrent+37]=this.st_8
this.Control[iCurrent+38]=this.em_fecha
this.Control[iCurrent+39]=this.st_9
this.Control[iCurrent+40]=this.st_10
this.Control[iCurrent+41]=this.st_11
this.Control[iCurrent+42]=this.em_dias
this.Control[iCurrent+43]=this.gb_4
this.Control[iCurrent+44]=this.st_1
this.Control[iCurrent+45]=this.st_12
this.Control[iCurrent+46]=this.rb_1
this.Control[iCurrent+47]=this.rb_2
this.Control[iCurrent+48]=this.rb_3
this.Control[iCurrent+49]=this.cbx_consolida
this.Control[iCurrent+50]=this.pb_pdf
this.Control[iCurrent+51]=this.dw_pdf
end on

on w_info_existencia_estadia_dias_pallet.destroy
call super::destroy
destroy(this.dw_cliente)
destroy(this.st_6)
destroy(this.st_4)
destroy(this.st_especie)
destroy(this.st_nro2)
destroy(this.st_variedad)
destroy(this.st_productor)
destroy(this.em_productor)
destroy(this.cb_buscaproductor)
destroy(this.sle_productor)
destroy(this.cbx_productor)
destroy(this.st_planta)
destroy(this.cbx_planta)
destroy(this.dw_planta)
destroy(this.gb_5)
destroy(this.st_5)
destroy(this.rb_embalaje)
destroy(this.rb_productor)
destroy(this.rb_variedad)
destroy(this.st_7)
destroy(this.cbx_consolplan)
destroy(this.cbx_consolprod)
destroy(this.rb_controltodos)
destroy(this.rb_rechazados)
destroy(this.rb_objetados)
destroy(this.rb_habilitado)
destroy(this.gb_6)
destroy(this.st_2)
destroy(this.uo_selcondicion)
destroy(this.cbx_predio)
destroy(this.st_3)
destroy(this.dw_stat)
destroy(this.cbx_status)
destroy(this.uo_selespecie)
destroy(this.uo_selvariedad)
destroy(this.cbx_varirotula)
destroy(this.st_8)
destroy(this.em_fecha)
destroy(this.st_9)
destroy(this.st_10)
destroy(this.st_11)
destroy(this.em_dias)
destroy(this.gb_4)
destroy(this.st_1)
destroy(this.st_12)
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.rb_3)
destroy(this.cbx_consolida)
destroy(this.pb_pdf)
destroy(this.dw_pdf)
end on

event open;call super::open;Boolean   lb_cerrar

// uo_seleccion_especie
IF IsNull(uo_selespecie.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selespecie.Seleccion(True,True)
END IF

// uo_seleccion_variedad
IF IsNull(uo_selvariedad.Codigo) THEN lb_Cerrar = True
IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selvariedad.Seleccion(True,True)
	uo_selvariedad.Enabled		=	False
END IF
	
IF IsNull(uo_selcondicion.Codigo) THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_selcondicion.Seleccion(True, True)
	uo_selcondicion.cbx_consolida.Visible = FALSE
	uo_selcondicion.cbx_consolida.Enabled = FALSE
	
	dw_cliente.GetChild("clie_codigo", idwc_cliente)
	idwc_cliente.SetTransObject(SQLCA)
	idwc_cliente.Retrieve()
	dw_cliente.InsertRow(0)
	dw_cliente.SetItem(1, "clie_codigo", gi_codexport)
	
	dw_planta.GetChild("plde_codigo", idwc_planta)
	idwc_planta.SetTransObject(SQLCA)
	idwc_planta.Retrieve(1)
	dw_planta.InsertRow(0)
	dw_planta.SetItem(1, "plde_codigo", gi_codplanta)
	
	istr_mant.argumento[1]	= 	String(gi_codexport)		//	Cliente
//	istr_mant.argumento[2]	= 	String(gi_codespecie)	//	Especie
//	istr_mant.argumento[3]	=	'0'							// Variedad
	istr_mant.argumento[5]	= 	'0'							//	Planta
	istr_mant.argumento[28] = 	'0'// Consolidado por Planta
	istr_mant.argumento[27] = 	'0'// Consolidado por Productor
	istr_mant.argumento[35]  =	'0'							// status
	istr_mant.argumento[36]	=  "Todos"						// status nombre
	
	is_NomPlanta				=	"TODAS"
	ii_control 					= 	-1
	is_control_d				=	"Todos"
	em_fecha.Text				=	String(Today())
END IF
end event

type pb_excel from w_para_informes`pb_excel within w_info_existencia_estadia_dias_pallet
end type

type st_computador from w_para_informes`st_computador within w_info_existencia_estadia_dias_pallet
integer x = 2976
integer y = 152
end type

type st_usuario from w_para_informes`st_usuario within w_info_existencia_estadia_dias_pallet
integer x = 2976
integer y = 80
end type

type st_temporada from w_para_informes`st_temporada within w_info_existencia_estadia_dias_pallet
integer x = 2976
integer y = 8
end type

type p_logo from w_para_informes`p_logo within w_info_existencia_estadia_dias_pallet
end type

type st_titulo from w_para_informes`st_titulo within w_info_existencia_estadia_dias_pallet
integer width = 3662
string text = "Existencia por Estadía por Días por Pallet"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_existencia_estadia_dias_pallet
string tag = "Imprimir Reporte"
integer x = 4041
integer y = 816
integer taborder = 120
end type

event pb_acepta::clicked;SetPointer(Arrow!)

Integer	Fila,li_Orden,li_Cond, li_varirotula, li_dias, li_tipopallet, li_consolpallet
String	l_s_Titulo,ls_Condicion,ls_planta,ls_productor
Date		ld_fecha

l_s_Titulo	=	'Existencia por Calidad'

istr_info.titulo	=	'Fruta Procesada - ' + l_s_Titulo

//Condición
IF uo_selcondicion.cbx_todos.checked THEN
   li_Cond	= -1
	ls_condicion = 'Todas'
ELSE
   li_Cond	= uo_selcondicion.codigo
   IF IsNull(li_Cond)THEN
	   MessageBox("Atención","Debe Seleccionar una Condición Previamente",Exclamation!)
	   RETURN
	ELSE
		ls_condicion   = uo_selcondicion.Nombre
	END IF
END IF

IF cbx_consolplan.CheCked THEN
	ls_Planta	=  "Consolidado"
ELSE	
   IF cbx_planta.CheCked  THEN
		ls_Planta	=  "Todos"
	ELSE
		ls_planta  =  is_NomPlanta
	END IF
END IF	
		
IF cbx_consolprod.CheCked THEN
	ls_Productor	=  "Consolidado"
ELSE	
   IF cbx_productor.CheCked  THEN
		ls_productor	=  "Todos"
	ELSE
		ls_productor  =  is_NomProductor
	END IF
END IF	

/*
Especies
*/
IF IsNull(uo_selespecie.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	uo_selespecie.dw_Seleccion.SetFocus()
	RETURN
END IF
/*
Variedad
*/
IF IsNull(uo_selvariedad.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Variedad Previamente",Exclamation!)
	uo_selvariedad.dw_Seleccion.SetFocus()
	RETURN
END IF

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

ld_fecha = Date(em_fecha.Text)

IF isnull(em_dias.Text) THEN
	li_dias = 0
ELSE
	li_dias	= Integer(em_dias.Text)
	
END IF	

IF rb_1.Checked THEN 
	li_tipopallet = 1
ELSEIF	rb_2.Checked THEN 
	li_tipopallet = 2
ELSEIF	rb_3.Checked THEN 
	li_tipopallet = -1
END IF	

IF cbx_consolida.Checked THEN
	li_consolpallet = -9
ELSE
	li_consolpallet = -1
END IF	

OpenWithParm(vinf, istr_info)

//IF cbx_predio.checked = True THEN
//	IF rb_embalaje.checked THEN
//		vinf.dw_1.DataObject = "dw_info_existencia_calibres_predio"
//		li_Orden = 1
//	ELSEIF rb_productor.checked THEN
//		vinf.dw_1.DataObject = "dw_info_exis_calibres_prod_predio"
//		li_Orden = 2
//	ELSE
//		vinf.dw_1.DataObject = "dw_info_existencia_calibres_var_predio"
//		li_Orden = 3
//	END IF
//ELSE
//	IF rb_embalaje.checked THEN
		vinf.dw_1.DataObject = "dw_info_existencia_estadía_dias_porpallet"
		li_Orden = 1
//	ELSEIF rb_productor.checked THEN
//		vinf.dw_1.DataObject = "dw_info_existencia_calibres_prod"
//		li_Orden = 2
//	ELSE
//		vinf.dw_1.DataObject = "dw_info_existencia_calibres_var"
//		li_Orden = 3
//	END IF
//END IF	
	
vinf.dw_1.SetTransObject(sqlca)

Fila	=	vinf.dw_1.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[5]), &
									 Long(istr_mant.argumento[4]),uo_selespecie.Codigo,uo_selvariedad.Codigo,&
									 li_Cond,li_Orden,Integer(istr_mant.argumento[27]),&
									 Integer(istr_mant.argumento[28]),ii_control,&
									 Integer(istr_mant.argumento[35]),li_varirotula,ld_fecha,li_dias,li_tipopallet,li_consolpallet)

IF Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	F_Membrete(vinf.dw_1)
	
	IF gs_Ambiente <> 'Windows' THEN
		F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
	END IF
END IF

SetPointer(Arrow!)
end event

type pb_salir from w_para_informes`pb_salir within w_info_existencia_estadia_dias_pallet
string tag = "Salir [Cerrar Ventana Activa]"
integer x = 4046
integer y = 1296
integer taborder = 130
string powertiptext = "Salir [Cerrar Ventana Activa]"
end type

type dw_cliente from datawindow within w_info_existencia_estadia_dias_pallet
integer x = 759
integer y = 460
integer width = 1184
integer height = 96
integer taborder = 20
boolean bringtotop = true
string dataobject = "dddw_clientesprod"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[1]	=	data

IF NoExisteCliente(istr_mant.argumento[1]) THEN
	dw_cliente.SetItem(1, "clie_codigo", gi_codexport)
	dw_cliente.SetFocus()
	RETURN 1
END IF

end event

event itemerror;RETURN 1
end event

type st_6 from statictext within w_info_existencia_estadia_dias_pallet
integer x = 343
integer y = 492
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Cliente"
boolean focusrectangle = false
end type

type st_4 from statictext within w_info_existencia_estadia_dias_pallet
integer x = 247
integer y = 440
integer width = 1742
integer height = 328
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_especie from statictext within w_info_existencia_estadia_dias_pallet
integer x = 343
integer y = 796
integer width = 238
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Especie"
boolean focusrectangle = false
end type

type st_nro2 from statictext within w_info_existencia_estadia_dias_pallet
integer x = 247
integer y = 764
integer width = 1742
integer height = 392
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_variedad from statictext within w_info_existencia_estadia_dias_pallet
integer x = 343
integer y = 1004
integer width = 279
integer height = 96
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Variedad"
boolean focusrectangle = false
end type

type st_productor from statictext within w_info_existencia_estadia_dias_pallet
integer x = 2085
integer y = 560
integer width = 297
integer height = 96
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Productor"
boolean focusrectangle = false
end type

type em_productor from editmask within w_info_existencia_estadia_dias_pallet
integer x = 2501
integer y = 544
integer width = 270
integer height = 96
integer taborder = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
boolean enabled = false
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#####"
end type

event modified;Long			ll_productor
Integer		li_cliente

IF This.Text <> '' AND This.Text <> '0' THEN
	ll_productor	=	Long(This.Text)
	li_cliente 		=  dw_cliente.Object.clie_codigo[1]
	
	SELECT	prod_nombre
		INTO	:is_NomProductor
		FROM	dba.productores as pro,dba.productoresclientes as cli
		WHERE	pro.prod_codigo =	:ll_productor
		AND	pro.prod_codigo = cli.prod_codigo
		AND	:li_cliente in (-1,cli.clie_codigo);
		
	IF sqlca.SQLCode = -1 THEN
		F_ErrorBaseDatos(sqlca, "Lectura de tabla Productores")
		This.SetFocus()
	ELSEIF sqlca.SQLCode = 100 THEN
	
		MessageBox("Atención", "Código de productor no ha sido definido o no existe para este cliente.~r~r" + &
			"Ingrese o seleccione otro Código.")
		em_productor.Text 	= ''
		sle_productor.Text 	= ''
		This.SetFocus()
	ELSE
		sle_productor.Text		=	is_NomProductor
		istr_mant.argumento[4]	=	String(ll_productor)
	END IF
END IF	
	
end event

type cb_buscaproductor from commandbutton within w_info_existencia_estadia_dias_pallet
integer x = 2784
integer y = 552
integer width = 96
integer height = 84
integer taborder = 110
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
boolean enabled = false
string text = "..."
end type

event clicked;Str_busqueda	lstr_busq

lstr_busq.argum[1]	=	istr_mant.argumento[1] // Cliente

OpenWithParm(w_busc_productores_clientes, lstr_busq)

lstr_busq	       = Message.PowerObjectParm

IF lstr_busq.argum[3] = "" THEN
	em_productor.SetFocus()
ELSE
	em_productor.Text			=	lstr_busq.argum[3]
	sle_productor.Text		=	lstr_busq.argum[4]
	istr_mant.argumento[4]	=	lstr_busq.argum[3]
END IF
end event

type sle_productor from singlelineedit within w_info_existencia_estadia_dias_pallet
integer x = 2885
integer y = 544
integer width = 987
integer height = 96
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
borderstyle borderstyle = stylelowered!
end type

type cbx_productor from checkbox within w_info_existencia_estadia_dias_pallet
integer x = 2505
integer y = 456
integer width = 402
integer height = 80
integer taborder = 90
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;istr_mant.argumento[27] = "0"
IF cbx_productor.Checked THEN
	em_productor.Enabled			=	False
	cb_buscaproductor.Enabled	=	False
	em_productor.Text				=	''
	sle_productor.Text			=	''
	istr_mant.argumento[4]		=	'0'
	cbx_consolprod.Checked		=  False
ELSE
	em_productor.Enabled			=	True
	cb_buscaproductor.Enabled	=	True
END IF
end event

type st_planta from statictext within w_info_existencia_estadia_dias_pallet
integer x = 343
integer y = 684
integer width = 233
integer height = 64
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Planta"
boolean focusrectangle = false
end type

type cbx_planta from checkbox within w_info_existencia_estadia_dias_pallet
integer x = 759
integer y = 568
integer width = 402
integer height = 80
integer taborder = 30
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;call super::clicked;
istr_mant.argumento[28] = "0"
IF cbx_planta.Checked THEN
	dw_planta.Enabled			=	False
	istr_mant.argumento[5]	=	'0'
	is_NomPlanta				=	"TODAS"
	cbx_consolplan.Checked  =  False
ELSE
	dw_planta.Enabled			=	True
	istr_mant.argumento[5]	=	String(dw_planta.GetItemNumber(1,"plde_codigo"))
	is_NomPlanta				=	idwc_planta.GetItemString(idwc_planta.GetRow(), "plde_nombre")
	
END IF
end event

type dw_planta from datawindow within w_info_existencia_estadia_dias_pallet
integer x = 759
integer y = 652
integer width = 969
integer height = 96
integer taborder = 40
boolean bringtotop = true
boolean enabled = false
string dataobject = "dddw_plantadesp_tipo"
boolean border = false
boolean livescroll = true
end type

event itemchanged;istr_mant.argumento[5]	=	data

IF NoExistePlanta(istr_mant.argumento[5]) THEN
	dw_planta.SetItem(1, "plde_codigo", gi_codplanta)
	is_NomPlanta				=	idwc_planta.GetItemString(idwc_planta.GetRow(), "plde_nombre")
	dw_planta.SetFocus()
	RETURN 1
END IF




end event

event itemerror;RETURN 1
end event

type gb_5 from groupbox within w_info_existencia_estadia_dias_pallet
boolean visible = false
integer x = 4128
integer y = 196
integer width = 1792
integer height = 168
integer taborder = 150
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Orden"
end type

type st_5 from statictext within w_info_existencia_estadia_dias_pallet
integer x = 1989
integer y = 436
integer width = 1920
integer height = 548
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_embalaje from radiobutton within w_info_existencia_estadia_dias_pallet
boolean visible = false
integer x = 4247
integer y = 240
integer width = 485
integer height = 100
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Por Embalaje"
boolean checked = true
end type

type rb_productor from radiobutton within w_info_existencia_estadia_dias_pallet
boolean visible = false
integer x = 5211
integer y = 1144
integer width = 485
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Por Productor"
end type

type rb_variedad from radiobutton within w_info_existencia_estadia_dias_pallet
boolean visible = false
integer x = 5577
integer y = 1124
integer width = 485
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Por Variedad"
end type

type st_7 from statictext within w_info_existencia_estadia_dias_pallet
integer x = 1989
integer y = 984
integer width = 1920
integer height = 116
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type cbx_consolplan from checkbox within w_info_existencia_estadia_dias_pallet
integer x = 1202
integer y = 568
integer width = 443
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidado"
end type

event clicked;istr_mant.argumento[28] = "0"
IF cbx_consolplan.Checked THEN
	dw_planta.Enabled			=	False
	istr_mant.argumento[5]	=	'0'
	is_NomPlanta				=	"TODAS"
	cbx_planta.Checked      =  False
	istr_mant.argumento[28] = "1"
ELSE
	dw_planta.Enabled			=	True
	istr_mant.argumento[5]	=	String(dw_planta.GetItemNumber(1,"plde_codigo"))
	is_NomPlanta				=	idwc_planta.GetItemString(idwc_planta.GetRow(), "plde_nombre")
END IF
end event

type cbx_consolprod from checkbox within w_info_existencia_estadia_dias_pallet
integer x = 2889
integer y = 456
integer width = 443
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidado"
end type

event clicked;istr_mant.argumento[27] = "0"
IF cbx_consolprod.Checked THEN
	em_productor.Enabled			=	False
	cb_buscaproductor.Enabled	=	False
	em_productor.Text				=	''
	sle_productor.Text			=	''
	istr_mant.argumento[4]		=	'0'
	cbx_productor.Checked		=  False
	istr_mant.argumento[27] = "1"
ELSE
	em_productor.Enabled			=	True
	cb_buscaproductor.Enabled	=	True
END IF
end event

type rb_controltodos from radiobutton within w_info_existencia_estadia_dias_pallet
integer x = 2144
integer y = 1180
integer width = 283
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Todos"
boolean checked = true
end type

event clicked;IF This.checked=True THEN
	ii_control	=	-1
	is_control_d= this.text
END IF
end event

type rb_rechazados from radiobutton within w_info_existencia_estadia_dias_pallet
integer x = 2501
integer y = 1180
integer width = 434
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Rechazados"
end type

event clicked;IF This.checked=True THEN
	ii_control	=	3
	is_control_d= this.text
END IF
end event

type rb_objetados from radiobutton within w_info_existencia_estadia_dias_pallet
integer x = 2930
integer y = 1180
integer width = 407
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Objetados"
end type

event clicked;IF This.checked=True THEN
	ii_control	=	2
	is_control_d= this.text
END IF
end event

type rb_habilitado from radiobutton within w_info_existencia_estadia_dias_pallet
integer x = 3378
integer y = 1180
integer width = 407
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Habilitados"
end type

event clicked;IF This.checked=True THEN
	ii_control	=	1
	is_control_d= this.text
END IF
end event

type gb_6 from groupbox within w_info_existencia_estadia_dias_pallet
integer x = 2075
integer y = 1116
integer width = 1769
integer height = 172
integer taborder = 140
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Control Calidad"
end type

type st_2 from statictext within w_info_existencia_estadia_dias_pallet
integer x = 2089
integer y = 736
integer width = 306
integer height = 96
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Condición"
boolean focusrectangle = false
end type

type uo_selcondicion from uo_seleccion_condicion within w_info_existencia_estadia_dias_pallet
integer x = 2496
integer y = 648
integer taborder = 120
boolean bringtotop = true
end type

on uo_selcondicion.destroy
call uo_seleccion_condicion::destroy
end on

type cbx_predio from checkbox within w_info_existencia_estadia_dias_pallet
boolean visible = false
integer x = 4201
integer y = 536
integer width = 343
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 12632256
string text = "Predio"
end type

type st_3 from statictext within w_info_existencia_estadia_dias_pallet
integer x = 2089
integer y = 892
integer width = 265
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Status"
boolean focusrectangle = false
end type

type dw_stat from datawindow within w_info_existencia_estadia_dias_pallet
boolean visible = false
integer x = 2473
integer y = 856
integer width = 969
integer height = 88
integer taborder = 120
boolean bringtotop = true
boolean enabled = false
string title = "none"
string dataobject = "dddw_status1"
boolean border = false
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

event itemchanged;istr_mant.argumento[6]	=	data
istr_mant.argumento[7]	=  f_statnombre(integer(data))

end event

type cbx_status from checkbox within w_info_existencia_estadia_dias_pallet
integer x = 2496
integer y = 876
integer width = 498
integer height = 80
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Consolidados    "
end type

event clicked;IF This.Checked THEN
   istr_mant.argumento[35]   =	'1'							// status
   istr_mant.argumento[36]	=  "Consolidados "			// status nombre
ELSE
   istr_mant.argumento[35]   =	'0'							// status
   istr_mant.argumento[36]	=  "Todos "						// status nombre
END IF	
//IF This.Checked THEN
//	dw_stat.Enabled			=	False
//	istr_mant.argumento[6]   =	'0'
//	istr_mant.argumento[7]	=  "Todos"	
//	dw_stat.Reset()
//	dw_stat.InsertRow(0)
//	dw_stat.Object.stat_codigo.BackGround.Color	=	RGB(166,180,210)
//	
//ELSE
//	dw_stat.Enabled			=	True	
//	dw_stat.Object.stat_codigo.BackGround.Color	=	RGB(255, 255, 255)
//	dw_stat.InsertRow(0)
//	dw_stat.SetItem(1, "stat_codigo", 1)
//	istr_mant.argumento[7]	=  "Normal"
//END IF
end event

type uo_selespecie from uo_seleccion_especie within w_info_existencia_estadia_dias_pallet
integer x = 759
integer y = 776
integer height = 180
integer taborder = 240
boolean bringtotop = true
end type

event ue_cambio;call super::ue_cambio;IF IsNull(This.Codigo) THEN RETURN

CHOOSE CASE This.Codigo
	CASE -1, -9
		uo_selvariedad.cbx_Todos.Checked			=	True
		uo_selvariedad.cbx_Consolida.Enabled	=	True
		uo_selvariedad.Enabled						=	False
	CASE ELSE
		uo_selvariedad.Filtra(This.Codigo)
		uo_selvariedad.Enabled						=	True
END CHOOSE
end event

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type uo_selvariedad from uo_seleccion_variedad within w_info_existencia_estadia_dias_pallet
integer x = 759
integer y = 964
integer taborder = 260
boolean bringtotop = true
end type

on uo_selvariedad.destroy
call uo_seleccion_variedad::destroy
end on

type cbx_varirotula from checkbox within w_info_existencia_estadia_dias_pallet
integer x = 2679
integer y = 1004
integer width = 576
integer height = 80
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Variedad Rotulada"
end type

type st_8 from statictext within w_info_existencia_estadia_dias_pallet
integer x = 247
integer y = 1160
integer width = 1742
integer height = 152
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type em_fecha from editmask within w_info_existencia_estadia_dias_pallet
integer x = 759
integer y = 1184
integer width = 402
integer height = 96
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type st_9 from statictext within w_info_existencia_estadia_dias_pallet
integer x = 343
integer y = 1200
integer width = 297
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Fecha Tope"
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_existencia_estadia_dias_pallet
integer x = 247
integer y = 1312
integer width = 1742
integer height = 176
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_11 from statictext within w_info_existencia_estadia_dias_pallet
integer x = 343
integer y = 1372
integer width = 389
integer height = 76
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Dias Superior a"
boolean focusrectangle = false
end type

type em_dias from editmask within w_info_existencia_estadia_dias_pallet
integer x = 759
integer y = 1352
integer width = 402
integer height = 96
integer taborder = 160
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string text = "none"
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "###"
end type

type gb_4 from groupbox within w_info_existencia_estadia_dias_pallet
integer x = 2075
integer y = 1328
integer width = 1769
integer height = 136
integer taborder = 150
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Tipo Pallet"
end type

type st_1 from statictext within w_info_existencia_estadia_dias_pallet
integer x = 1989
integer y = 1100
integer width = 1920
integer height = 216
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_12 from statictext within w_info_existencia_estadia_dias_pallet
integer x = 1989
integer y = 1316
integer width = 1920
integer height = 172
integer textsize = -10
integer weight = 700
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
boolean enabled = false
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type rb_1 from radiobutton within w_info_existencia_estadia_dias_pallet
integer x = 2240
integer y = 1380
integer width = 343
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Pallet"
end type

type rb_2 from radiobutton within w_info_existencia_estadia_dias_pallet
integer x = 2720
integer y = 1380
integer width = 343
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Pucho"
end type

type rb_3 from radiobutton within w_info_existencia_estadia_dias_pallet
integer x = 3269
integer y = 1380
integer width = 343
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 33543637
string text = "Ambas"
boolean checked = true
end type

type cbx_consolida from checkbox within w_info_existencia_estadia_dias_pallet
integer x = 1330
integer y = 1368
integer width = 544
integer height = 72
boolean bringtotop = true
integer textsize = -8
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 33543637
string text = "Consolida Pallet"
boolean lefttext = true
end type

type pb_pdf from picturebutton within w_info_existencia_estadia_dias_pallet
integer x = 4046
integer y = 1060
integer width = 233
integer height = 196
integer taborder = 150
boolean bringtotop = true
integer textsize = -10
integer weight = 400
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
string picturename = "\Desarrollo 12\Imagenes\Botones\pdfe.bmp"
string disabledname = "\Desarrollo 12\Imagenes\Botones\pdfd.bmp"
alignment htextalign = right!
end type

event clicked;SetPointer(Arrow!)

Integer	Fila,li_Orden,li_Cond, li_varirotula, li_dias, li_tipopallet, li_consolpallet
String	l_s_Titulo,ls_Condicion,ls_planta,ls_productor,ls_DirectorioAct, ls_Archivo
Date		ld_fecha

l_s_Titulo	=	'Existencia por Calidad'

istr_info.titulo	=	'Fruta Procesada - ' + l_s_Titulo

//Condición
IF uo_selcondicion.cbx_todos.checked THEN
   li_Cond	= -1
	ls_condicion = 'Todas'
ELSE
   li_Cond	= uo_selcondicion.dw_Seleccion.Object.codigo[1]
   IF IsNull(li_Cond)THEN
	   MessageBox("Atención","Debe Seleccionar una Condición Previamente",Exclamation!)
	   RETURN
	ELSE
		ls_condicion   = uo_selcondicion.Nombre
	END IF
END IF

IF cbx_consolplan.CheCked THEN
	ls_Planta	=  "Consolidado"
ELSE	
   IF cbx_planta.CheCked  THEN
		ls_Planta	=  "Todos"
	ELSE
		ls_planta  =  is_NomPlanta
	END IF
END IF	
		
IF cbx_consolprod.CheCked THEN
	ls_Productor	=  "Consolidado"
ELSE	
   IF cbx_productor.CheCked  THEN
		ls_productor	=  "Todos"
	ELSE
		ls_productor  =  is_NomProductor
	END IF
END IF	

/*
Especies
*/
IF IsNull(uo_selespecie.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Especie Previamente",Exclamation!)
	uo_selespecie.dw_Seleccion.SetFocus()
	RETURN
END IF
/*
Variedad
*/
IF IsNull(uo_selvariedad.Codigo)THEN
	MessageBox("Atención","Debe Seleccionar una Variedad Previamente",Exclamation!)
	uo_selvariedad.dw_Seleccion.SetFocus()
	RETURN
END IF

IF cbx_varirotula.Checked THEN
	li_varirotula = 1
ELSE
	li_varirotula = 0
END IF

ld_fecha = Date(em_fecha.Text)

IF isnull(em_dias.Text) THEN
	li_dias = 0
ELSE
	li_dias	= Integer(em_dias.Text)
	
END IF	

IF rb_1.Checked THEN 
	li_tipopallet = 1
ELSEIF	rb_2.Checked THEN 
	li_tipopallet = 2
ELSEIF	rb_3.Checked THEN 
	li_tipopallet = -1
END IF	

IF cbx_consolida.Checked THEN
	li_consolpallet = -9
ELSE
	li_consolpallet = -1
END IF	

		li_Orden = 1
	
dw_pdf.SetTransObject(sqlca)

Fila	=	dw_pdf.Retrieve(Integer(istr_mant.argumento[1]),Integer(istr_mant.argumento[5]), &
									 Long(istr_mant.argumento[4]),uo_selespecie.Codigo,uo_selvariedad.Codigo,&
									 li_Cond,li_Orden,Integer(istr_mant.argumento[27]),&
									 Integer(istr_mant.argumento[28]),ii_control,&
									 Integer(istr_mant.argumento[35]),li_varirotula,ld_fecha,li_dias,li_tipopallet,li_consolpallet)

IF Fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
			   	"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)

ELSEIF Fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	             StopSign!, Ok!)

ELSE
	F_Membrete(dw_pdf)
//	str_mant	lstr_mant
//	
//	lstr_mant.dw	=	dw_pdf
//	
//	OpenWithParm (w_imprimir_pdf, lstr_mant)
	
	dw_pdf.Object.DataWindow.Export.PDF.Method = Distill! 
	dw_pdf.Object.DataWindow.Export.PDF.Distill.CustomPostScript="Yes" 
//	dw_pdf.Modify("Export.PDF.Method = XSLFOP! ")
	dw_pdf.Modify("datawindow.Export.PDF.Method = '1'") 
//	dw_pdf.Object.DataWindow.Printer = "Sybase DataWindow PS" 
//	dw_pdf.Modify("datawindow.Export.PDF.xslfop.print=no") 
	
	dw_pdf.SaveAs("C:prueba.pdf", PDF!, true) 

	
//RegistryGet( "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders", "Personal", RegString!, ls_DirectorioAct)
//
//ls_Archivo	=	ls_DirectorioAct + '\existenciaestadiapallet'+String(today(),'ddmmyyyy')+'.pdf'
//
//is_asunto = 'PDF Existencia Estadia Pallet por Día'
//
//is_archivo = ls_Archivo
//
//dw_pdf.SaveAs(is_archivo, PDF!  , false)
//	
//enviamail()

END IF

SetPointer(Arrow!)
end event

type dw_pdf from datawindow within w_info_existencia_estadia_dias_pallet
boolean visible = false
integer x = 3963
integer y = 1584
integer width = 686
integer height = 400
integer taborder = 170
boolean bringtotop = true
string title = "none"
string dataobject = "dw_info_existencia_estadía_dias_porpallet"
boolean livescroll = true
borderstyle borderstyle = stylelowered!
end type

