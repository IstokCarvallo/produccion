$PBExportHeader$w_mant_deta_spro_ordenventacomdeta.srw
forward
global type w_mant_deta_spro_ordenventacomdeta from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_spro_ordenventacomdeta from w_mant_detalle_csd
integer width = 2665
integer height = 1700
end type
global w_mant_deta_spro_ordenventacomdeta w_mant_deta_spro_ordenventacomdeta

type variables
uo_especie 			 iuo_especie
uo_variedades 		 iuo_variedad
uo_grupoespecie    iuo_grupo
uo_subgrupoespecie iuo_subgrupo
uo_categorias		 iuo_categoria	
uo_tratamientofrio iuo_tratamientofrio

Datawindowchild    idwc_planta, idwc_especie, idwc_grupo, idwc_subgrupo, idwc_variedad, &
                   idwc_categoria, idwc_frio, idwc_tipoenva, idwc_envase
end variables

forward prototypes
public subroutine buscaprecios ()
public subroutine buscavalor (string as_columna, string as_valor)
end prototypes

public subroutine buscaprecios ();String  ls_Nula
Integer li_especie
Str_Busqueda	lstr_busq

SetNull(ls_Nula)

lstr_busq.argum[1] = istr_Mant.Argumento[5]
li_especie 	=	dw_1.Object.espe_codigo[il_fila]
IF isnull(li_especie) or li_especie=0 THEN
	lstr_busq.argum[2] = "0"
ELSE	
	lstr_busq.argum[2] = String(li_especie)
END IF

OpenWithParm(w_busc_lista_precios, lstr_busq)

lstr_busq	=	Message.PowerObjectParm

IF lstr_busq.argum[1] <> "" THEN
	
	dw_1.SetItem(il_fila,"espe_codigo",integer(lstr_busq.argum[1]))
	dw_1.SetItem(il_fila,'grva_codigo', Integer(ls_Nula))  
	dw_1.GetChild("grva_codigo",idwc_grupo)
	idwc_grupo.SetTransObject(Sqlca)
	idwc_grupo.Retrieve(integer(lstr_busq.argum[1]))
			/**/
	dw_1.SetItem(il_fila,'grva_codsub', Integer(ls_Nula))
	dw_1.GetChild("grva_codsub",idwc_subgrupo)
	idwc_subgrupo.SetTransObject(Sqlca)
	idwc_subgrupo.Retrieve(integer(lstr_busq.argum[1]),0)
			/**/
	dw_1.SetItem(il_fila,'vari_codigo', Integer(ls_Nula))
	dw_1.GetChild("vari_codigo",idwc_variedad)
	idwc_variedad.SetTransObject(Sqlca)
	idwc_variedad.Retrieve(integer(lstr_busq.argum[1]))
	
	IF lstr_busq.argum[2]<>"" THEN
		dw_1.SetItem(il_fila,"grva_codigo",integer(lstr_busq.argum[2]))
		
		IF NOT iuo_Grupo.Existe(integer(lstr_busq.argum[1]),Integer(lstr_busq.argum[2]),True,SqlCa) THEN
			dw_1.SetItem(il_fila, "grva_codigo", Integer(ls_Nula))
			dw_1.SetFocus()
		ELSE
			dw_1.SetItem(il_fila, "grva_nombre_grupo", iuo_Grupo.nombregrupo)
			dw_1.SetItem(il_fila,'grva_codsub', Integer(ls_Nula))
			dw_1.SetItem(il_fila,'vari_codigo', Integer(ls_Nula))
			dw_1.GetChild("grva_codsub",idwc_subgrupo)
			idwc_subgrupo.SetTransObject(Sqlca)
			idwc_subgrupo.Retrieve(integer(lstr_busq.argum[1]),Integer(lstr_busq.argum[2]))
		
			idwc_variedad.SetFilter("grva_codigo=" + lstr_busq.argum[2])
			idwc_variedad.Filter()
		END IF
	END IF
	
	IF lstr_busq.argum[3]<>"" THEN
		dw_1.SetItem(il_fila,"grva_codsub",integer(lstr_busq.argum[3]))
		IF NOT iuo_SubGrupo.Existe(integer(lstr_busq.argum[1]),Integer(lstr_busq.argum[2]), &
											Integer(lstr_busq.argum[3]),True,SqlCa) THEN
			dw_1.SetItem(il_fila, "grva_codsub", integer(ls_Nula))
			dw_1.SetFocus()
		ELSE	
			dw_1.SetItem(il_fila, "grva_nombre_subgrupo", iuo_SubGrupo.nombregrupo)
			dw_1.SetItem(il_fila,'vari_codigo', Integer(ls_Nula))
			idwc_variedad.SetFilter("grva_codigo=" + lstr_busq.argum[2] + &
											" And grva_codsub=" + lstr_busq.argum[3])
			idwc_variedad.Filter()
		END IF
	END IF 
	
	IF lstr_busq.argum[4]<>"" THEN
		IF iuo_variedad.existe(Integer(lstr_busq.argum[1]),integer(lstr_busq.argum[4]),True,SQLCA) THEN
			
			dw_1.SetItem(il_fila,"vari_codigo",integer(lstr_busq.argum[4]))
			dw_1.SetItem(il_fila, "vari_nombre", iuo_variedad.nombrevariedad)
		END IF	
	END IF
	
	dw_1.SetItem(il_fila,"cate_codigo",integer(lstr_busq.argum[5]))
	
	dw_1.SetItem(il_fila,"ofcd_gcalib",lstr_busq.argum[6])
	
	dw_1.SetItem(il_fila,"ofcd_valuni",long(lstr_busq.argum[7]))
	
	
END IF




end subroutine

public subroutine buscavalor (string as_columna, string as_valor);Integer	li_especie, li_grupo, li_subgrupo, li_variedad, li_categoria
Long		ll_valor
Date		ld_fecha
String	ls_gcalibre

ld_fecha	=	Date(Mid(istr_Mant.Argumento[5],1,10))
li_especie 	=	dw_1.Object.espe_codigo[il_fila]
li_grupo 	=	dw_1.Object.grva_codigo[il_fila]
li_subgrupo	=	dw_1.Object.grva_codsub[il_fila]
li_variedad = 	dw_1.Object.vari_codigo[il_fila]
ls_gcalibre	=	dw_1.Object.ofcd_gcalib[il_fila]
li_Categoria=  dw_1.Object.cate_codigo[il_fila]

CHOOSE CASE as_columna
		
	CASE "espe_codigo"
		
		li_especie 	= integer(as_valor)
		
	CASE "grva_codigo"
		
		li_grupo	 	= integer(as_valor)
		
	CASE "grva_codsub"
		
		li_subgrupo = integer(as_valor)
		
	CASE "vari_codigo"
		
		li_variedad = integer(as_valor)
	
	CASE "ofcd_gcalib"
		
		ls_gcalibre 	= as_valor

	CASE "cate_codigo"
		
		li_categoria = integer(as_valor)
		
END CHOOSE		

SELECT tafc_preuni INTO :ll_valor
 FROM dba.spro_tarifafrutacomercial
WHERE espe_codigo 	=:	li_especie
  AND :li_grupo  		in (grva_codigo, Null)
  AND :li_subgrupo   in (grva_codsub, Null)  
  AND :li_variedad	in (vari_codigo, Null)
  AND :ls_gcalibre	in (refe_gcalib, Null)
  AND :li_Categoria	in (cate_codigo, Null)
  AND :ld_fecha between tafc_fecham and tafc_fechat;
  
IF SqlCa.SQLCode = -1 THEN
	F_ErrorBaseDatos(SqlCa, "Lectura de Tabla spro_tarifafrutacomercial")
ELSEIF SqlCa.SQLCode <> 100 THEN
		dw_1.Object.ofcd_valuni[il_fila] = ll_valor
END IF	
end subroutine

on w_mant_deta_spro_ordenventacomdeta.create
call super::create
end on

on w_mant_deta_spro_ordenventacomdeta.destroy
call super::destroy
end on

event open;
x	= 100
y	= 450

This.Icon	=	Gstr_apl.Icono

istr_mant = Message.PowerObjectParm

iuo_especie				=	Create uo_especie
iuo_variedad			=	Create uo_variedades
iuo_grupo				=	Create uo_grupoespecie
iuo_subgrupo			=	Create uo_subgrupoespecie
iuo_categoria			=	Create uo_categorias
iuo_tratamientofrio  =  Create uo_tratamientofrio
//planta
dw_1.Getchild("plde_codigo",idwc_planta)
idwc_planta.SetTransObject(SQLCA)
idwc_planta.Retrieve()

//Especie
dw_1.GetChild("espe_codigo",idwc_especie)
idwc_especie.SetTransObject(SQLCA)
idwc_especie.Retrieve()

//Grupo
dw_1.Getchild("grva_codigo",idwc_grupo)
idwc_grupo.SetTransObject(SQLCA)
idwc_grupo.insertrow(0)


//SubGrupo
dw_1.Getchild("grva_codsub",idwc_subgrupo)
idwc_subgrupo.SetTransObject(SQLCA)
idwc_subgrupo.insertrow(0)

//Variedad
dw_1.Getchild("vari_codigo",idwc_variedad)
idwc_variedad.SetTransObject(SQLCA)
idwc_variedad.insertrow(0)


//Categoria
dw_1.GetChild("cate_codigo",idwc_categoria)
idwc_categoria.SetTransObject(SQLCA)
idwc_categoria.Retrieve()
idwc_categoria.SetFilter("cate_embalada <> 1")
idwc_categoria.Filter()

//Frio
dw_1.GetChild("frio_tipofr",idwc_frio)
idwc_frio.SetTransObject(SQLCA)
idwc_frio.Retrieve()

//Tipo de Envase
dw_1.GetChild("enva_tipoen",idwc_tipoenva)
idwc_tipoenva.SetTransObject(SQLCA)
idwc_tipoenva.Retrieve()
idwc_tipoenva.SetFilter("tien_usoenv = 1")
idwc_tipoenva.Filter()

//Grupo
dw_1.Getchild("enva_codigo",idwc_envase)
idwc_envase.SetTransObject(SQLCA)
IF idwc_envase.Retrieve(0)=0 THEN
   idwc_envase.insertrow(0)
END IF 

PostEvent("ue_recuperadatos")

IF istr_mant.argumento[7] = "1" THEN
	dw_1.Object.ofcd_bultos.visible  =	1
	dw_1.Object.ofcd_tkilos.visible  =	0
	dw_1.Object.t_bultos.visible		=	1
	dw_1.Object.t_kilos.visible		=	0
ELSE
	dw_1.Object.ofcd_bultos.visible  =	0
	dw_1.Object.ofcd_tkilos.visible  =	1
	dw_1.Object.t_bultos.visible		=	0
	dw_1.Object.t_kilos.visible		=	1
	
END IF	
	
dw_1.SetTransObject(sqlca)
istr_mant.dw.ShareData(dw_1)
end event

event ue_antesguardar();call super::ue_antesguardar;Integer	li_cont
String	ls_mensaje, ls_colu[]

IF Isnull(dw_1.Object.espe_codigo[il_fila]) OR dw_1.Object.espe_codigo[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nCódigo de Especie"
	ls_colu[li_cont]	= "espe_codigo"
END IF

//IF Isnull(dw_1.Object.cate_codigo[il_fila]) OR dw_1.Object.cate_codigo[il_fila] = 0 THEN
//   li_cont ++
//	ls_mensaje 			= ls_mensaje + "~nCódigo de Categoria"
//	ls_colu[li_cont]	= "cate_codigo"
//END IF
//
IF Isnull(dw_1.Object.frio_tipofr[il_fila]) OR dw_1.Object.frio_tipofr[il_fila] = "" THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nTratamiento Frío"
	ls_colu[li_cont]	= "frio_tipofr"
END IF

IF Isnull(dw_1.Object.enva_tipoen[il_fila]) OR dw_1.Object.espe_codigo[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nTipo de Envase"
	ls_colu[li_cont]	= "enva_tipoen"
END IF

IF Isnull(dw_1.Object.enva_codigo[il_fila]) OR dw_1.Object.enva_codigo[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nEnvase"
	ls_colu[li_cont]	= "enva_codigo"
END IF

//IF Isnull(dw_1.Object.ofcd_gcalib[il_fila]) OR dw_1.Object.ofcd_gcalib[il_fila] = "" THEN
//   li_cont ++
//	ls_mensaje 			= ls_mensaje + "~nGrupo Calibre"
//	ls_colu[li_cont]	= "ofcd_gcalib"
//END IF

IF istr_mant.argumento[7] = "1" THEN
	IF Isnull(dw_1.Object.ofcd_bultos[il_fila]) OR dw_1.Object.ofcd_bultos[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nCantidad de Bultos"
		ls_colu[li_cont]	= "ofcd_bultos"
	END IF
ELSE
	IF Isnull(dw_1.Object.ofcd_tkilos[il_fila]) OR dw_1.Object.ofcd_tkilos[il_fila] = 0 THEN
		li_cont ++
		ls_mensaje 			= ls_mensaje + "~nKilos"
		ls_colu[li_cont]	= "ofcd_tkilos"
	END IF
END IF	

IF Isnull(dw_1.Object.ofcd_valuni[il_fila]) OR dw_1.Object.ofcd_valuni[il_fila] = 0 THEN
   li_cont ++
	ls_mensaje 			= ls_mensaje + "~nValor Unitario"
	ls_colu[li_cont]	= "ofcd_valuni"
END IF

IF li_cont > 0 THEN
	MessageBox("Error de Consistencia", "Falta el ingreso de : " + ls_mensaje + ".", StopSign!, Ok!)
	dw_1.SetColumn(ls_colu[1])
	dw_1.SetFocus() 
	Message.DoubleParm = -1
END IF
end event

event ue_deshace();call super::ue_deshace;
IF upperBound(ias_campo) > 0 THEN
	dw_1.Object.espe_codigo[il_fila]	=	integer(ias_campo[1])
	dw_1.Object.grva_codigo[il_fila]	=	integer(ias_campo[2])
	dw_1.Object.grva_codsub[il_fila]	=	integer(ias_campo[3])
	dw_1.Object.vari_codigo[il_fila]	=	integer(ias_campo[4])
	dw_1.Object.cate_codigo[il_fila]	=	integer(ias_campo[5])
	dw_1.Object.frio_tipofr[il_fila]	=	ias_campo[6]
	dw_1.Object.ofcd_gcalib[il_fila]	=	ias_campo[7]
	dw_1.Object.ofcd_bultos[il_fila]	=	Long(ias_campo[8])
	dw_1.Object.ofcd_valuni[il_fila]	=	Long(ias_campo[9])
	dw_1.Object.enva_tipoen[il_fila]	=	integer(ias_campo[10])
	dw_1.Object.enva_codigo[il_fila]	=	integer(ias_campo[11])
END IF
end event

event ue_nuevo();ib_ok = True

This.TriggerEvent("ue_guardar")

IF Message.DoubleParm = -1 THEN ib_ok = False

IF ib_ok = False THEN RETURN

wf_nuevo()

dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
dw_1.SetItem(il_fila, "odfc_numero", Integer(istr_mant.argumento[3]))

dw_1.SetColumn("espe_codigo")
dw_1.SetFocus()
end event

event ue_recuperadatos();call super::ue_recuperadatos;ias_campo[1]	=	String(dw_1.Object.espe_codigo[il_fila])
ias_campo[2]	=	String(dw_1.Object.grva_codigo[il_fila])
ias_campo[3]	=	String(dw_1.Object.grva_codsub[il_fila])
ias_campo[4]	=	String(dw_1.Object.vari_codigo[il_fila])
ias_campo[5]	=	String(dw_1.Object.cate_codigo[il_fila])
ias_campo[6]	=	String(dw_1.Object.frio_tipofr[il_fila])
ias_campo[7]	=	String(dw_1.Object.ofcd_gcalib[il_fila])
ias_campo[8]	=	String(dw_1.Object.ofcd_bultos[il_fila])
ias_campo[9]	=	String(dw_1.Object.ofcd_valuni[il_fila])
ias_campo[10]	=	String(dw_1.Object.enva_tipoen[il_fila])
ias_campo[11]	=	String(dw_1.Object.enva_codigo[il_fila])

IF ias_campo[10]<>"" THEN
	dw_1.GetChild("enva_codigo",idwc_envase)
	idwc_envase.SetTransObject(SQLCA)
	IF idwc_envase.Retrieve(integer(ias_campo[10]))=0 THEN
		idwc_envase.InsertRow(0)
	END IF	
END IF	

IF istr_mant.agrega THEN
	dw_1.SetItem(il_fila, "plde_codigo", Integer(istr_mant.argumento[1]))
	dw_1.SetItem(il_fila, "odfc_numero", Integer(istr_mant.argumento[3]))
ELSE
	dw_1.Getchild("grva_codigo",idwc_grupo)
	idwc_grupo.SetTransObject(SQLCA)
	IF idwc_grupo.Retrieve(integer(ias_campo[1]),0)=0 THEN
   	idwc_grupo.insertrow(0)
	END IF 

	//SubGrupo
	dw_1.Getchild("grva_codsub",idwc_subgrupo)
	idwc_subgrupo.SetTransObject(SQLCA)
	IF idwc_subgrupo.Retrieve(integer(ias_campo[1]),integer(ias_campo[2]))=0 THEN
		idwc_subgrupo.insertrow(0)
	END IF	

	//Variedad
	dw_1.Getchild("vari_codigo",idwc_variedad)
	idwc_variedad.SetTransObject(SQLCA)
	IF idwc_variedad.Retrieve(integer(ias_campo[1]))=0 THEN
		idwc_variedad.insertrow(0)
	END IF
END IF


end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_spro_ordenventacomdeta
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_spro_ordenventacomdeta
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_spro_ordenventacomdeta
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_spro_ordenventacomdeta
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_spro_ordenventacomdeta
integer x = 2359
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_spro_ordenventacomdeta
integer x = 2359
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_spro_ordenventacomdeta
integer x = 2359
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_spro_ordenventacomdeta
integer x = 50
integer y = 64
integer width = 2181
integer height = 1280
string dataobject = "dw_mant_spro_ordenventacomdeta"
end type

event dw_1::itemchanged;Integer	li_Null, li_tipoenvase
String	ls_Columna, ls_Nula
str_envase lstr_envase

ls_Columna = dwo.Name

SetNull(li_Null)
SetNull(ls_Nula)

CHOOSE CASE ls_Columna
		
	CASE "espe_codigo"	
		IF NOT iuo_Especie.Existe(Integer(Data),True,SqlCa) THEN  
			This.SetItem(row,"espe_codigo", integer(ls_Nula))
			This.SetItem(row,'grva_codigo', Integer(ls_Nula))
			This.SetItem(row,'grva_codsub', Integer(ls_Nula))
			This.SetItem(row,'vari_codigo', Integer(ls_Nula))			
			This.SetFocus()
			RETURN 1
	   ELSE
			this.SetItem(row,'grva_codigo', Integer(ls_Nula))  
			dw_1.GetChild("grva_codigo",idwc_grupo)
			idwc_grupo.SetTransObject(Sqlca)
			idwc_grupo.Retrieve(integer(data))
			/**/
	   	this.SetItem(row,'grva_codsub', Integer(ls_Nula))
			dw_1.GetChild("grva_codsub",idwc_subgrupo)
			idwc_subgrupo.SetTransObject(Sqlca)
			idwc_subgrupo.Retrieve(integer(data),0)
			/**/
			this.SetItem(row,'vari_codigo', Integer(ls_Nula))
			dw_1.GetChild("vari_codigo",idwc_variedad)
			idwc_variedad.SetTransObject(Sqlca)
			idwc_variedad.Retrieve(integer(data))
			
			BuscaValor(ls_columna,Data)
		END IF	
	

	CASE "grva_codigo"
		
		IF NOT iuo_Grupo.Existe(dw_1.Object.espe_codigo[il_fila],Integer(data),True,SqlCa) THEN
			This.SetItem(il_fila, "grva_codigo", Integer(ls_Nula))
			This.SetFocus()
			RETURN 1
		ELSE
			This.SetItem(il_fila, "grva_nombre_grupo", iuo_Grupo.nombregrupo)
			this.SetItem(il_fila,'grva_codsub', Integer(ls_Nula))
			this.SetItem(il_fila,'vari_codigo', Integer(ls_Nula))
			dw_1.GetChild("grva_codsub",idwc_subgrupo)
			idwc_subgrupo.SetTransObject(Sqlca)
			idwc_subgrupo.Retrieve(dw_1.Object.espe_codigo[il_fila],integer(data))
		
			idwc_variedad.SetFilter("grva_codigo=" + Data)
			idwc_variedad.Filter()
			
			BuscaValor(ls_columna,Data)
		END IF	
		
	CASE "grva_codsub"
		IF NOT iuo_SubGrupo.Existe(dw_1.Object.espe_codigo[il_fila],dw_1.Object.grva_codigo[il_fila],Integer(data),True,SqlCa) THEN
			This.SetItem(il_fila, "grva_codsub", integer(ls_Nula))
			This.SetFocus()
			RETURN 1
		ELSE	
			This.SetItem(il_fila, "grva_nombre_subgrupo", iuo_SubGrupo.nombregrupo)
			this.SetItem(il_fila,'vari_codigo', Integer(ls_Nula))
			idwc_variedad.SetFilter("grva_codigo=" + String(dw_1.Object.grva_codigo[il_fila]) + &
		                        " And grva_codsub=" + Data)
			idwc_variedad.Filter()
			BuscaValor(ls_columna,Data)  
		END IF
		
	CASE "vari_codigo"
		
		IF NOT iuo_variedad.existe(dw_1.Object.espe_codigo[il_fila],integer(data),True,SQLCA) THEN
			This.SetItem(il_fila, "vari_codigo", integer(ls_Nula))
			This.SetFocus()
			RETURN 1
		ELSE
			IF NOT iuo_Grupo.Existe(dw_1.Object.espe_codigo[il_fila],iuo_variedad.grupo,True,SqlCa) THEN
				This.SetItem(il_fila, "grva_codigo", iuo_variedad.grupo)
				This.SetItem(il_fila, "grva_nombre_grupo", iuo_Grupo.nombregrupo)
			END IF	
			
			IF NOT iuo_SubGrupo.Existe(dw_1.Object.espe_codigo[il_fila],iuo_variedad.grupo,iuo_variedad.subgrupo,True,SqlCa) THEN
				This.SetItem(il_fila, "grva_codsub", iuo_variedad.subgrupo)
				This.SetItem(il_fila, "grva_nombre_subgrupo", iuo_SubGrupo.nombregrupo)
			END IF
			
			This.SetItem(il_fila, "vari_nombre", iuo_variedad.nombrevariedad)
			BuscaValor(ls_columna,Data)
		END IF
		
	CASE "cate_codigo"
		IF Not iuo_categoria.existe(integer(data),true,SQLCA) THEN
			This.SetItem(il_fila, "cate_codigo", integer(ls_Nula))
			This.SetFocus()
			RETURN 1
		ELSE
			Buscavalor(ls_columna,Data)
		END IF	
	
	CASE "ofcd_gcalib"
		BuscaValor(ls_columna,Data)
	
	CASE "enva_tipoen"
		IF NOT Existeenvase(integer(data),0,lstr_envase) THEN
			This.SetItem(il_fila, "enva_tipoen", integer(ls_Nula))
			This.SetFocus()
			RETURN 1
		ELSEIF lstr_envase.UsoEnvase = 1 THEN
			this.GetChild("enva_codigo",idwc_envase)
			idwc_envase.SetTransObject(SQLCA)
			IF idwc_envase.ReTrieve(integer(data))=0 THEN
				idwc_envase.InsertRow(0)
			END IF
		ELSE
			MessageBox("Atención","Debe Seleccionar un Tipo de Envase Cosechero.")
			This.SetItem(il_fila, "enva_tipoen", integer(ls_Nula))
			This.SetFocus()
			RETURN 1
		END IF	
		
	CASE "enva_codigo"
		li_tipoenvase=dw_1.Object.enva_tipoen[il_fila]
		IF isnull(li_tipoenvase) or li_tipoenvase=0 THEN
			messagebox("Falta de Datos","Debe Seleccionar un Tipo de Envase Primero.")
			This.SetItem(il_fila, "enva_codigo", integer(ls_Nula))
			This.SetFocus()
			RETURN 1
		ELSE
			IF NOT Existeenvase(li_tipoenvase,integer(data),lstr_envase) THEN
				This.SetItem(il_fila, "enva_codigo", integer(ls_Nula))
				This.SetFocus()
				RETURN 1
			END IF
		END IF
		
	CASE "frio_tipofr"
		IF Not iuo_tratamientofrio.ofp_recupera_tratamientofrio(SQLCA,data,True) THEN
			This.SetItem(il_fila, "frio_tipofr", ls_Nula)
			This.SetFocus()
			RETURN 1
		END IF		
		
END CHOOSE


end event

event dw_1::buttonclicked;call super::buttonclicked;CHOOSE CASE dwo.Name

	CASE "b_precios"
		Buscaprecios()

END CHOOSE
end event

